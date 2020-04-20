{-|

This module exports the functionality to construct probability values correctly.

For this small project, it adds some unnecessary boilerplate,
but in larger codebases it may be smart to constrain constructing correct values like this.

By using this module, it's impossible to use a 'Probability' value
that is beyond the range of 0 <= p > 1.

By using a 'newtype' we avoid any runtime performance penalty for using a custom data type.

 -}

module Bank.Probability
  ( Probability -- Do not export constructor (..) here so invalid values can't be used to construct
  , probability -- This is what is used to create the type
  , getProb -- for unpacking the probability value
  )
where
-- Dependency imports
import           Control.Exception              ( assert )

newtype Probability = Probability {getProb :: Double}
  deriving (Show, Eq)

-- | A smart constructor for building probability value.
-- Faulty inputs can be handled here without needing to pollute
-- every receiving function with value checking & error handling logic.
probability :: Double -> Probability
probability n = assert (n >= 0 && n < 1) $ Probability n
