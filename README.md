# priority-applicative

Applicatives with priorities

## Usage

`Control.Applicative.Prio` allows controlling the order of execution of [`Applicative`][1] effects
with priorities. Effects are executed in order of increasing priority. If two priorities are equal,
either effect may be executed first.

A simple example:

```hs
import Control.Applicative.Prio (PrioAp)
import qualified Control.Applicative.Prio as PrioAp

prioPrint :: (Ord k, Show k, Show a) => k -> a -> PrioAp k IO a
prioPrint k x =
  PrioAp.lift k (x <$ putStrLn ("priority " ++ show k ++ ": " ++ show x))

foo :: IO (Int, (String, Bool), Double, Char)
foo = PrioAp.unlift $
  (,,,) <$>
    prioPrint 3 42 <*>
    ((,) <$> prioPrint 1 "hello" <*> prioPrint 5 False) <*>
    prioPrint 4 3.14 <*>
    prioPrint 2 'p'
```

When `foo` is run, its value will be `(42,("hello",False),3.14,'p')` as expected.
But it will print out the following:

```
priority 1: "hello"
priority 2: 'p'
priority 3: 42
priority 4: 3.14
priority 5: False
```

`Control.Alternative.Prio` extends the behavior to [`Alternative`][2]. Alternative branches are
chosen in order of the minimum priority effect in the branch.

```hs
import qualified Control.Alternative.Prio as PrioAlt

bar :: IO ()
bar = PrioAlt.unlift $
  (PrioAlt.lift 4 (print 4) *> PrioAlt.lift 2 (print 2)) <|>
  (PrioAlt.lift 1 (print 1) *> PrioAlt.lift 3 empty)
```

The minimum priorities in the two branches are 2 and 1. So the second is chosen first. When the
second branch fails, it switches to the first branch.  
The output of running `bar` is:

```
1
2
4
```

## More examples

### Parsing in priority order

```hs
import Control.Alternative.Prio as PrioAlt

data Name = Name LastName FirstName
data Person = Person (Either Name Nickname) Age Mail

pFirstName :: Parser FirstName
pLastName  :: Parser LastName
pNickname  :: Parser Nickname
pAge       :: Parser Age
pMail      :: Parser Mail

-- The data is present in sorted order:
-- age, firstName, lastName, mail OR
-- age, mail, nickname

pPerson :: Parser Person
pPerson = PrioAlt.unlift $
  Person <$>
    ((Left . Name <$>
        PrioAlt.lift "lastName" pLastName <*>
        PrioAlt.lift "firstName" pFirstName) <|>
     Right <$> PrioAlt.lift "nickname" pNickname) <*>
    PrioAlt.lift "age" pAge <*>
    PrioAlt.lift "mail" pMail
```

Without `PrioAlt` one would need to
* Write an fmap function to swap around the arguments, instead of a plain constructor
* Manually branch the parser after `age`

### Sorting

```hs
import Control.Monad.Trans.Writer.Lazy  -- from transformers
import qualified Control.Applicative.Prio as PrioAp

sort :: Ord a => [a] -> [a]
sort = execWriter . PrioAp.unlift . traverse_ (\x -> PrioAp.lift x (tell [x]))
```

### Non-empty heap

```hs
import Data.Functor.Const
import Control.Applicative.Prio (PrioAp, Some(..))
import qualified Control.Applicative.Prio as PrioAp

type Heap p a = Some (PrioAp p (Const a))

singleton :: p -> a -> Heap p a
singleton p x = Some (PrioAp.lift p (Const x))

union :: Ord p => Heap p a -> Heap p a -> Heap p a
union (Some h1) (Some h2) = Some (h1 *> h2)

minView :: Heap p a -> Maybe ((p, a), Heap p a)
minView (Some h) = fmap f (PrioAp.minView h)
  where
    f ((p, Some (Const x)), h) = ((p, x), h)
```

## Performance

The current implementation is backed by a sorted list for `PrioAp` and sorted tree for `PrioAlt`.
This makes it simple and easy to understand, but it's not very efficient. I have some ideas on how
to improve performance.

## Acknowledgements

This package uses the technique used in [`Control.Applicative.Permutations`][3] in the
`parser-combinators` package, which is in turn inspired by the functional pearl, [_Parsing
Permutation Phrases_][4], by Arthur Baars, Andres Löh and S. Doaitse Swierstra.

[1]: https://hackage.haskell.org/package/base/docs/Control-Applicative.html#t:Applicative
[2]: https://hackage.haskell.org/package/base/docs/Control-Applicative.html#t:Alternative
[3]: https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/Control-Applicative-Permutations.html
[4]: https://www.cs.ox.ac.uk/jeremy.gibbons/wg21/meeting56/loeh-paper.pdf
