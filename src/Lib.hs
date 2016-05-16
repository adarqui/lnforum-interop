{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib (
) where

import           Lib2
import           LN.T
import           Haskell.Interop.Prime

mkExports
  (Options
    (defaultOptionsCleanPurescript "/tmp/LN.purs")
    (MkGHeader "import Purescript.Api.Helpers\n" : (defaultPurescriptMkGs "module LN.T.Internal.Types where"))
    (defaultOptions_Haskell_adarqui "/tmp/LN.hs")
    (MkGHeader "import LN.T.Internal.Types\nimport Data.Int\n" : (defaultHaskellMkGs $ tplTestHeader "LN.T.Internal.JSON")))
  [ f ''ApiRequest
  , f ''ApiResponse
  , f ''ApiResponses

  , f ''BoardRequest
  , f ''BoardResponse
  , f ''BoardResponses
  , f ''BoardStatResponse
  , f ''BoardStatResponses

  , f ''BucketRequest
  , f ''BucketResponse
  , f ''BucketResponses

  , f ''CountResponse
  , f ''CountResponses

  , f ''DepList

  , f ''EmptyRequest
  , f ''EmptyResponse
  , f ''EmptyResponses

  , f ''Entity

  , f ''ForumRequest
  , f ''ForumResponse
  , f ''ForumResponses
  , f ''ForumStatResponse
  , f ''ForumStatResponses

  , f ''LeuronRequest
  , f ''LeuronResponse
  , f ''LeuronResponses

  , f ''LeuronStatResponse
  , f ''LeuronStatResponses

  , f ''LeuronLikeRequest
  , f ''LeuronLikeResponse
  , f ''LeuronLikeResponses

  , f ''LeuronLikeStatResponse
  , f ''LeuronLikeStatResponses

  , f ''LeuronStarRequest
  , f ''LeuronStarResponse
  , f ''LeuronStarResponses

  , f ''LeuronStarStatResponse
  , f ''LeuronStarStatResponses

  , f ''LikeOpt
  , f ''LikeRequest
  , f ''LikeResponse
  , f ''LikeResponses
  , f ''LikeStatResponse
  , f ''LikeStatResponses

  , f ''LeuronData
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

  , f ''OrganizationRequest
  , f ''OrganizationResponse
  , f ''OrganizationResponses
  , f ''OrganizationStatResponse
  , f ''OrganizationStatResponses

  , f_without_show ''Param
  , f_without_show ''ParamTag
  , f_without_show ''SortOrderBy
  , f_without_show ''OrderBy

  , f ''PmRequest
  , f ''PmResponse
  , f ''PmResponses

  , f ''PmInRequest
  , f ''PmInResponse
  , f ''PmInResponses

  , f ''PmOutRequest
  , f ''PmOutResponse
  , f ''PmOutResponses

  , f ''ProfileX
  , f_with_eq ''ProfileGender
  , f ''ProfileRequest
  , f ''ProfileResponse
  , f ''ProfileResponses

  , f ''ReminderRequest
  , f ''ReminderResponse
  , f ''ReminderResponses

  , f ''ReminderFolderRequest
  , f ''ReminderFolderResponse
  , f ''ReminderFolderResponses

  , f ''ResourceType
  , f ''ResourceRequest
  , f ''ResourceResponse
  , f ''ResourceResponses

  , f ''ResourceStatResponse
  , f ''ResourceStatResponses

  , f ''ResourceLikeRequest
  , f ''ResourceLikeResponse
  , f ''ResourceLikeResponses

  , f ''ResourceLikeStatResponse
  , f ''ResourceLikeStatResponses

  , f ''ResourceStarRequest
  , f ''ResourceStarResponse
  , f ''ResourceStarResponses

  , f ''ResourceStarStatResponse
  , f ''ResourceStarStatResponses

  , f_with_eq ''Size

  , f ''Splits
  , f ''Substitutions

--  , f ''StyleResponse
--  , f ''StyleResponses
--
  , f ''StarRequest
  , f ''StarResponse
  , f ''StarResponses
  , f ''StarStatResponse
  , f ''StarStatResponses

  , f ''TeamRequest
  , f ''TeamResponse
  , f ''TeamResponses
  , f ''TeamStatResponse
  , f ''TeamStatResponses

  , f ''TestRequest
  , f ''TestResponse
  , f ''TestResponses

  , f ''ThreadRequest
  , f ''ThreadResponse
  , f ''ThreadResponses

  , f ''ThreadStatResponse
  , f ''ThreadStatResponses

  , f ''PostData

  , f ''ThreadPostRequest
  , f ''ThreadPostResponse
  , f ''ThreadPostResponses

  , f ''ThreadPostStatResponse
  , f ''ThreadPostStatResponses

  , f ''ThreadPostLikeRequest
  , f ''ThreadPostLikeResponse
  , f ''ThreadPostLikeResponses

  , f ''ThreadPostLikeStatResponse
  , f ''ThreadPostLikeStatResponses

  , f ''ThreadPostStarRequest
  , f ''ThreadPostStarResponse
  , f ''ThreadPostStarResponses

  , f ''ThreadPostStarStatResponse
  , f ''ThreadPostStarStatResponses

  , f ''UserRequest
  , f ''UserResponse
  , f ''UserResponses
  , f ''UserSanitizedResponse
  , f ''UserSanitizedResponses
  , f ''UserSanitizedStatResponse
  , f ''UserSanitizedStatResponses

  , f ''Visibility

{-
  , f ''WorkoutRequest
  , f ''WorkoutResponse
  , f ''WorkoutResponses
  , f ''Workout
  , f ''WorkoutRest
  , f ''WorkoutData
  , f ''ExerciseType
  , f ''ExerciseMeasurement
  , f ''ExerciseInfo
  , f ''TemplateInfo
-}

  -- Packs
  --
  , f ''OrganizationPackResponse
  , f ''OrganizationPackResponses

  , f ''TeamPackResponse
  , f ''TeamPackResponses

  , f ''UserPackResponse
  , f ''UserPackResponses

  , f ''UserSanitizedPackResponse
  , f ''UserSanitizedPackResponses

  , f ''ForumPackResponse
  , f ''ForumPackResponses

  , f ''BoardPackResponse
  , f ''BoardPackResponses

  , f ''ThreadPackResponse
  , f ''ThreadPackResponses

  , f ''ThreadPostPackResponse
  , f ''ThreadPostPackResponses

  , f ''ResourcePackResponse
  , f ''ResourcePackResponses

  , f ''LeuronPackResponse
  , f ''LeuronPackResponses
  ]



mkApi
  (Options
    ((defaultOptionsCleanPurescript "/tmp/LN.Api.purs") { debug = True })
    (MkGHeader "import Prelude\nimport Data.Either\nimport LN.T.Internal.Types\n" : (defaultPurescriptApiMkGs "module LN.Api.Internal where"))
    ((defaultOptionsCleanHaskell "/tmp/LN.Api.hs" ) { debug = True })
    (MkGHeader "import LN.T\nimport Data.Int\n" : (defaultHaskellApiMkGs $ tplTestHeader "LN.Api.Internal")))
  apiSpec_TH



mkApi
  (Options
    ((defaultOptionsCleanPurescript "/tmp/LN.Api.String.purs") { debug = True })
    (MkGHeader "import Prelude\nimport Data.Either\nimport LN.T.Internal.Types\n" : (defaultPurescriptApiMkGs "module LN.Api.Internal.String where"))
    ((defaultOptionsCleanHaskell "/tmp/LN.Api.String.hs" ) { debug = True })
    (MkGHeader "import LN.T\nimport Data.Int\n" : (defaultHaskellApiMkGs $ tplTestHeader "LN.Api.Internal.String")))
  apiSpec_String_TH
