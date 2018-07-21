{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module LibForum (
) where



import           Haskell.Interop.Prime
import           Prelude

import           LN.T

import           LibForum2



{-
f_mkType'
  ["LN.T.Permission"]
  "LN.T.ACL" "LN/T/ACL" $
  [ f_withBoth ''ACL [MkEq] ]
-}



f_mkType "LN.T.Api" "LN/T/Api" $
  [ f ''ApiRequest
  , f ''ApiResponse
  , f ''ApiResponses
  ]



f_mkType "LN.T.Count" "LN/T/Count" $
  [ f ''CountResponse
  , f ''CountResponses
  ]



f_mkType "LN.T.Ent" "LN/T/Ent" $
  [ f_withBoth ''Ent [MkEq, MkRead] ]



f_mkType "LN.T.Id" "LN/T/Id" $
  [ f ''IdRequest
  , f ''IdResponse
  , f ''IdResponses
  ]



f_mkType "LN.T.Error" "LN/T/Error" $
  [ f_withBoth ''ApplicationError [MkEq, MkDefault "Error_Unknown"]
  , f_withBoth ''ValidationError [MkEq, MkDefault "Validate Validate_Unknown Nothing"]
  , f_withBoth ''ValidationErrorCode [MkEq, MkDefault "Validate_Unknown"]
  ]



f_mkType'
  ["LN.T.Ent"]
  "LN.T.Like" "LN/T/Like" $
  [ f_withBoth ''LikeOpt [MkEq, MkRead]
  , f ''LikeRequest
  , f ''LikeResponse
  , f ''LikeResponses
  , f ''LikeStatResponse
  , f ''LikeStatResponses
  ]



f_mkType "LN.T.Param" "LN/T/Param" $
  [ f_withBoth ''Param [MkEq, MkShow, MkQueryParam]
  , f_withBoth' ''ParamTag [MkTypeOpts_Deriving Deriving_Ord] [MkEq, MkShow, MkRead]
  , f_withBoth ''SortOrderBy [MkEq, MkShow, MkRead]
  , f_withBoth ''OrderBy [MkEq, MkShow, MkRead]
  ]



f_mkType "LN.T.Permission" "LN/T/Permission" $
  [ f_withBoth ''Permission [MkEq]
  , f ''Permissions
  ]



f_mkType' ["LN.T.Ent"] "LN.T.Profile" "LN/T/Profile" $
  [ f ''ProfileX
  , f_withBoth ''ProfileGender [MkEq, MkRead]
  , f ''ProfileRequest
  , f ''ProfileResponse
  , f ''ProfileResponses
  ]



f_mkType' ["LN.T.Visibility"] "LN.T.Board" "LN/T/Board" $
  [ f ''BoardType
  , f_withBoth ''TyBoardType [MkEq, MkShow]
  , f ''BoardRequest
  , f ''BoardResponse
  , f ''BoardResponses
  , f ''BoardStatResponse
  , f ''BoardStatResponses
  ]



f_mkType "LN.T.Size" "LN/T/Size" $
  [ f_withBoth ''Size [MkEq] ]



f_mkType' ["LN.T.Ent"] "LN.T.Star" "LN/T/Star" $
  [ f ''StarRequest
  , f ''StarResponse
  , f ''StarResponses
  , f ''StarStatResponse
  , f ''StarStatResponses
  ]



f_mkType "LN.T.User" "LN/T/User" $
  [ f ''UserRequest
  , f ''UserResponse
  , f ''UserResponses
  , f ''UserSanitizedResponse
  , f ''UserSanitizedResponses
  , f ''UserSanitizedStatResponse
  , f ''UserSanitizedStatResponses
  ]



f_mkType "LN.T.Visibility" "LN/T/Visibility" $
  [ f_withBoth ''Visibility [MkEq, MkRead, MkShow] ]



--
-- Packs
--



f_mkType'
  ["LN.T.Profile", "LN.T.User"]
  "LN.T.Pack.User" "LN/T/Pack/User" $
  [ f ''UserPackResponse
  , f ''UserPackResponses
  ]



f_mkType'
  ["LN.T.User", "LN.T.Like", "LN.T.Star", "LN.T.Profile"]
  "LN.T.Pack.Sanitized.User" "LN/T/Pack/Sanitized/User" $
  [ f ''UserSanitizedPackResponse
  , f ''UserSanitizedPackResponses
  ]



f_mkType'
  ["LN.T.Board", "LN.T.User", "LN.T.Permission", "LN.T.Like", "LN.T.Star"]
  "LN.T.Pack.Board" "LN/T/Pack/Board" $
  [ f ''BoardPackResponse
  , f ''BoardPackResponses
  ]



mkConvert
  (Options
    (defaultOptionsCleanPurescript "../purescript-lnforum-types/src/LN/T/Convert.purs")
    (MkGHeader "import LN.T\n" : (defaultPurescriptConvertMkGs "module LN.T.Convert where"))
    (defaultOptions_Haskell_adarqui "../haskell-lnforum-types/src/LN/T/Convert.hs")
    (MkGHeader "import LN.T\n" : (defaultHaskellConvertMkGs $ tplTestHeader "LN.T.Convert")))
  [ (''ApiRequest, ''ApiResponse)
  , (''ApiResponse, ''ApiRequest)

  , (''IdRequest, ''IdResponse)
  , (''IdResponse, ''IdRequest)

  , (''ProfileRequest, ''ProfileResponse)
  , (''ProfileResponse, ''ProfileRequest)

  , (''BoardRequest, ''BoardResponse)
  , (''BoardResponse, ''BoardRequest)

  , (''UserRequest, ''UserResponse)
  , (''UserResponse, ''UserRequest)

  , (''UserRequest, ''UserSanitizedResponse)
  , (''UserSanitizedResponse, ''UserRequest)
  ]



mkApi
  (Options
    ((defaultOptionsCleanPurescript "../purescript-lnforum-api/src/LN/Api.purs") { debug = True })
    (MkGHeader "import LN.T\n" : (defaultPurescriptApiMkGs "module LN.Api where"))
    ((defaultOptionsCleanHaskell "../lnforum-api/src/LN/Api.hs" ) { debug = True })
    (MkGHeader haskellApiImports : (defaultHaskellApiMkGs $ tplTestHeader "LN.Api")))
  ''ApplicationError
  apiSpec_TH



mkApi
  (Options
    ((defaultOptionsCleanPurescript "../purescript-lnforum-api/src/LN/Api/String.purs") { debug = True })
    (MkGHeader "import LN.T\n" : (defaultPurescriptApiStringMkGs "module LN.Api.String where"))
    ((defaultOptionsCleanHaskell "../lnforum-api/src/LN/Api/String.hs" ) { debug = True })
    (MkGHeader haskellApiImports : (defaultHaskellApiStringMkGs $ tplTestHeader "LN.Api.String")))
  ''ApplicationError
  apiSpec_String_TH