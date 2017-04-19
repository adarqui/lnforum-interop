{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module LibNotes (
) where



import           Haskell.Interop.Prime
import           Prelude

import           LN.T

import           LibNotes2



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



f_mkType "LN.T.Bucket" "LN/T/Bucket" $
  [ f ''BucketRequest
  , f ''BucketResponse
  , f ''BucketResponses
  ]



f_mkType "LN.T.BucketRound" "LN/T/BucketRound" $
  [ f ''BucketRoundRequest
  , f ''BucketRoundResponse
  , f ''BucketRoundResponses
  ]



f_mkType "LN.T.BucketNode" "LN/T/BucketNode" $
  [ f ''BucketNodeRequest
  , f ''BucketNodeResponse
  , f ''BucketNodeResponses
  ]



f_mkType "LN.T.Count" "LN/T/Count" $
  [ f ''CountResponse
  , f ''CountResponses
  ]



f_mkType "LN.T.DepList" "LN/T/DepList" $
  [ f ''DepList ]



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
  ["LN.T.DepList", "LN.T.Splits", "LN.T.Substitutions"]
  "LN.T.Leuron" "LN/T/Leuron" $
  [ f ''LeuronRequest
  , f ''LeuronResponse
  , f ''LeuronResponses
  , f ''LeuronStatResponse
  , f ''LeuronStatResponses
  , f ''LeuronData
  , f_withBoth ''TyLeuron [MkEq, MkShow]
  , f ''Fact
  , f ''FactList
  , f ''Card
  , f ''DCard
  , f ''DCardX
  , f ''Acronym
  , f ''Synonym
  , f ''Antonym
  , f ''Template
  , f ''TemplateValue
  , f ''ImageAssociation
  , f ''Script
  , f ''LDContent
  , f ''LDHint
  , f ''LinearDemoNode
  , f ''LinearDemo
  , f ''QA
  , f ''Table
  ]



f_mkType "LN.T.LeuronTraining" "LN/T/LeuronTraining" $
  [ f_withBoth ''LeuronTrainingSummary [MkEq, MkRead]
  , f ''LeuronTrainingRequest
  , f ''LeuronTrainingResponse
  , f ''LeuronTrainingResponses
  , f ''LeuronTrainingStatResponse
  , f ''LeuronTrainingStatResponses
  ]



f_mkType "LN.T.LeuronNode" "LN/T/LeuronNode" $
  [ f ''LeuronNodeRequest
  , f ''LeuronNodeResponse
  , f ''LeuronNodeResponses
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



f_mkType' ["LN.T.DepList", "LN.T.Visibility"] "LN.T.Resource" "LN/T/Resource" $
  [ f ''ResourceType
  , f_withBoth ''TyResourceType [MkEq, MkShow]
  , f ''ResourceRequest
  , f ''ResourceResponse
  , f ''ResourceResponses
  , f ''ResourceStatResponse
  , f ''ResourceStatResponses
  ]



f_mkType "LN.T.Simple" "LN/T/Simple" $
  [ f ''SimpleIntRequest
  , f ''SimpleIntResponse
  , f ''SimpleIntsRequest
  , f ''SimpleIntsResponse
  , f ''SimpleStringRequest
  , f ''SimpleStringResponse
  , f ''SimpleStringsRequest
  , f ''SimpleStringsResponse
  ]



f_mkType "LN.T.Size" "LN/T/Size" $
  [ f_withBoth ''Size [MkEq] ]



f_mkType "LN.T.Splits" "LN/T/Splits" $
  [ f ''Splits
  , f_withBoth ''TySplits [MkEq]
  ]



f_mkType "LN.T.Substitutions" "LN/T/Substitutions" $
  [ f ''Substitutions
  , f_withBoth ''TySubstitutions [MkEq]
  ]



--  , f ''StyleResponse
--  , f ''StyleResponses



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
  ["LN.T.Resource", "LN.T.User", "LN.T.Permission", "LN.T.Like", "LN.T.Star"]
  "LN.T.Pack.Resource" "LN/T/Pack/Resource" $
  [ f ''ResourcePackResponse
  , f ''ResourcePackResponses
  ]



f_mkType'
  ["LN.T.Leuron", "LN.T.LeuronTraining", "LN.T.User", "LN.T.Permission", "LN.T.Star", "LN.T.Like"]
  "LN.T.Pack.Leuron" "LN/T/Pack/Leuron" $
  [ f ''LeuronPackResponse
  , f ''LeuronPackResponses
  ]



f_mkType'
  ["LN.T.Bucket","LN.T.User"]
  "LN.T.Pack.Bucket" "LN/T/Pack/Bucket" $
  [ f ''BucketPackResponse
  , f ''BucketPackResponses
  ]



f_mkType'
  ["LN.T.Resource", "LN.T.Leuron", "LN.T.LeuronTraining", "LN.T.Bucket"]
  "LN.T.Templates" "LN/T/Templates" $
  [ f ''Templates ]



mkConvert
  (Options
    (defaultOptionsCleanPurescript "../purescript-lnotes-types/src/LN/T/Convert.purs")
    (MkGHeader "import LN.T\n" : (defaultPurescriptConvertMkGs "module LN.T.Convert where"))
    (defaultOptions_Haskell_adarqui "../haskell-lnotes-types/src/LN/T/Convert.hs")
    (MkGHeader "import LN.T\n" : (defaultHaskellConvertMkGs $ tplTestHeader "LN.T.Convert")))
  [ (''ApiRequest, ''ApiResponse)
  , (''ApiResponse, ''ApiRequest)

  , (''BucketRequest, ''BucketResponse)
  , (''BucketResponse, ''BucketRequest)

  , (''BucketRoundRequest, ''BucketRoundResponse)
  , (''BucketRoundResponse, ''BucketRoundRequest)

  , (''BucketNodeRequest, ''BucketNodeResponse)
  , (''BucketNodeResponse, ''BucketNodeRequest)

  , (''IdRequest, ''IdResponse)
  , (''IdResponse, ''IdRequest)

  , (''LeuronRequest, ''LeuronResponse)
  , (''LeuronResponse, ''LeuronRequest)

  , (''LeuronTrainingRequest, ''LeuronTrainingResponse)
  , (''LeuronTrainingResponse, ''LeuronTrainingRequest)

  , (''LeuronNodeRequest, ''LeuronNodeResponse)
  , (''LeuronNodeResponse, ''LeuronNodeRequest)

  , (''ProfileRequest, ''ProfileResponse)
  , (''ProfileResponse, ''ProfileRequest)

  , (''ResourceRequest, ''ResourceResponse)
  , (''ResourceResponse, ''ResourceRequest)

  , (''UserRequest, ''UserResponse)
  , (''UserResponse, ''UserRequest)

  , (''UserRequest, ''UserSanitizedResponse)
  , (''UserSanitizedResponse, ''UserRequest)

  , (''SimpleStringRequest, ''SimpleStringResponse)
  , (''SimpleStringsRequest, ''SimpleStringsResponse)
  , (''SimpleStringRequest, ''SimpleStringResponse)
  , (''SimpleStringsRequest, ''SimpleStringsResponse)
  ]



mkApi
  (Options
    ((defaultOptionsCleanPurescript "../purescript-lnotes-api/src/LN/Api.purs") { debug = True })
    (MkGHeader "import LN.T\n" : (defaultPurescriptApiMkGs "module LN.Api where"))
    ((defaultOptionsCleanHaskell "../lnotes-api/src/LN/Api.hs" ) { debug = True })
    (MkGHeader haskellApiImports : (defaultHaskellApiMkGs $ tplTestHeader "LN.Api")))
  ''ApplicationError
  apiSpec_TH



mkApi
  (Options
    ((defaultOptionsCleanPurescript "../purescript-lnotes-api/src/LN/Api/String.purs") { debug = True })
    (MkGHeader "import LN.T\n" : (defaultPurescriptApiStringMkGs "module LN.Api.String where"))
    ((defaultOptionsCleanHaskell "../lnotes-api/src/LN/Api/String.hs" ) { debug = True })
    (MkGHeader haskellApiImports : (defaultHaskellApiStringMkGs $ tplTestHeader "LN.Api.String")))
  ''ApplicationError
  apiSpec_String_TH
