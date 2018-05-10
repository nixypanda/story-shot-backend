{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module Resource.Tag
  ( getTags
  , getTag
  , createTags
  , createTag
  , updateTags
  , updateTag
  , deleteTags
  , deleteTag
  ) where


import qualified Init as I
import qualified Type.Pagination as TP
import qualified Type.Doc as TD
import qualified Type.Tag as TT
import qualified Type.Meta as TM
import qualified Storage.Tag as ST



-- CREATE

createTag :: TT.TagInsert -> I.AppT TT.Tag
createTag = ST.createTag


createTags :: [TT.TagInsert] -> I.AppT [TT.Tag]
createTags = ST.createTags



-- RETRIVE


getTags :: TP.CursorParam -> I.AppT [TT.Tag]
getTags = ST.getTags


getTag :: Int -> I.AppT (Maybe TT.Tag)
getTag = ST.getTag



-- UPDATE

updateTag :: TT.TagPut -> I.AppT (Maybe TT.Tag)
updateTag = ST.updateTag


updateTags :: [TT.TagPut] -> I.AppT [TT.Tag]
updateTags = ST.updateTags



-- DELETE

deleteTag :: Int -> I.AppT Int
deleteTag = fmap fromIntegral . ST.deleteTag


deleteTags :: [Int] -> I.AppT Int
deleteTags = fmap fromIntegral . ST.deleteTags
