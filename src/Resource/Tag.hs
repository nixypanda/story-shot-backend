{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module Resource.Tag
  ( getTagResources
  , getTagResource
  , createTagResources
  , createTagResource
  , updateTagResources
  , updateTagResource
  , deleteTagResources
  , deleteTagResource
  ) where


import qualified Init as I
import qualified Type.Pagination as TP
import qualified Type.Doc as TD
import qualified Type.Tag as TT
import qualified Type.Meta as TM
import qualified Storage.Tag as ST



-- CREATE

createTagResource :: TT.TagInsert -> I.AppT (TD.Doc TT.Tag)
createTagResource = fmap TM.indexDoc' . ST.createTag


createTagResources :: [TT.TagInsert] -> I.AppT (TD.Doc TT.Tag)
createTagResources = fmap TM.docMulti . ST.createTags



-- RETRIVE

getTagResources :: TP.CursorParam -> I.AppT (TD.Doc TT.Tag)
getTagResources = fmap TM.docMulti . ST.getTags


getTagResource :: Int -> I.AppT (TD.MaybeResource TT.Tag)
getTagResource = fmap TM.docOrError . ST.getTag



-- UPDATE

updateTagResource :: TT.TagPut -> I.AppT (TD.MaybeResource TT.Tag)
updateTagResource = fmap TM.docOrError . ST.updateTag


updateTagResources :: [TT.TagPut] -> I.AppT (TD.Doc TT.Tag)
updateTagResources = fmap TM.docMulti . ST.updateTags



-- DELETE

deleteTagResource :: Int -> I.AppT (TD.MaybeResource TT.Tag)
deleteTagResource = fmap TM.docMetaOrError . ST.deleteTag


deleteTagResources :: [Int] -> I.AppT (TD.Doc TT.Tag)
deleteTagResources = fmap (TM.docMeta . fromIntegral) . ST.deleteTags
