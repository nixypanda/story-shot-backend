module Domain.Types.Author
    ( Author
    ) where


import qualified Data.Text as Text
import qualified Data.Time as DT


data Author =
  Author
    { authorID   :: Int
    , authorName :: Text.Text
    , createdAt  :: DT.UTCTime
    , updatedAt  :: DT.UTCTime
    } deriving (Eq, Show)

