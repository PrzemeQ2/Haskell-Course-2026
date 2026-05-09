newtype Reader r a = Reader { runReader :: r -> a}

-- 1. Functor, Applicative, and Monad instances
instance Functor (Reader r) where
    -- fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
    -- pure   :: a -> Reader r a
    pure a = Reader(\_ -> a)
    -- liftA2 :: (a -> b -> c) -> Reader r a -> Reader r b -> Reader r c
    liftA2 f ra rb = Reader (\r -> f (runReader ra r) (runReader rb r))

instance Monad (Reader r) where
    -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (>>=) (Reader f) g = Reader (\r -> runReader (g (f r)) r)

-- 2. Primitive operations

ask :: Reader r r
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader f

local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader g) = Reader (g . f)

-- 3. A practical example - banking system

data BankConfig = BankConfig
  { interestRate   :: Double  -- annual interest rate (e.g. 0.05 for 5%)
  , transactionFee :: Int     -- flat fee charged per transaction
  , minimumBalance :: Int     -- minimum required balance on an account
  } deriving (Show)

data Account = Account
  { accountId :: String       -- account identifier
  , balance   :: Int          -- current balance
  } deriving (Show)

-- Computes the interest accrued on the account, based on the configured rate.
-- The result should be an Int — round or truncate as you see fit, but be consistent.
calculateInterest :: Account -> Reader BankConfig Int
calculateInterest account = do 
    let bal = balance account
    rate <- asks interestRate 
    return (round (rate * (fromIntegral bal)))

-- Deducts the transaction fee from the account and returns the updated account.
-- The accountId should remain unchanged.
applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee account = do 
    fee <- asks transactionFee
    return account { balance = balance account - fee }

-- Checks whether the account balance meets the configured minimum.
checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance account = do 
    let currentBalance = balance account
    minBalance <- asks minimumBalance 
    return (currentBalance >= minBalance)

-- Runs the three operations above on a single account and combines their results.
-- The returned tuple contains:
--   * the account after the transaction fee has been applied,
--   * the interest computed from the ORIGINAL account,
--   * whether the ORIGINAL account meets the minimum balance requirement.
processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount account = do 
    processedAccount <- applyTransactionFee account
    originalRate <- calculateInterest account
    criteria <- checkMinimumBalance account 
    return (processedAccount, originalRate, criteria)

-- ghci> cfg = BankConfig { interestRate = 0.05, transactionFee = 2, minimumBalance = 100 }
-- ghci> acc = Account { accountId = "A-001", balance = 1000 }
-- ghci> runReader (processAccount acc) cfg
-- (Account {accountId = "A-001", balance = 998},50,True)