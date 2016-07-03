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
    (MkGHeader "import LN.T.Internal.Types\n" : (defaultHaskellMkGs $ tplTestHeader "LN.T.Internal.JSON")))
  [ f_withBoth ''ACL [MkEq]

  , f ''ApiRequest
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

  , f_withBoth ''Ent [MkEq, MkRead]

  , f_withBoth ''ApplicationError [MkEq]
  , f_withBoth ''ValidationError [MkEq]
  , f_withBoth ''ValidationErrorCode [MkEq]

  , f ''ForumRequest
  , f ''ForumResponse
  , f ''ForumResponses
  , f ''ForumStatResponse
  , f ''ForumStatResponses

  , f ''GlobalGroupRequest
  , f ''GlobalGroupResponse
  , f ''GlobalGroupResponses
  , f ''GlobalGroupStatResponse
  , f ''GlobalGroupStatResponses

  , f ''GroupRequest
  , f ''GroupResponse
  , f ''GroupResponses
  , f ''GroupStatResponse
  , f ''GroupStatResponses

  , f ''GroupMemberRequest
  , f ''GroupMemberResponse
  , f ''GroupMemberResponses
  , f ''GroupMemberStatResponse
  , f ''GroupMemberStatResponses

  , f ''LeuronRequest
  , f ''LeuronResponse
  , f ''LeuronResponses

  , f ''LeuronStatResponse
  , f ''LeuronStatResponses

  , f_withBoth ''LeuronTrainingSummary [MkEq, MkRead]
  , f ''LeuronTrainingRequest
  , f ''LeuronTrainingResponse
  , f ''LeuronTrainingResponses

  , f ''LeuronTrainingStatResponse
  , f ''LeuronTrainingStatResponses

  , f_withBoth ''LikeOpt [MkEq, MkRead]
  , f ''LikeRequest
  , f ''LikeResponse
  , f ''LikeResponses
  , f ''LikeStatResponse
  , f ''LikeStatResponses

  , f ''LeuronData
  , f_withBoth ''TyLeuron [MkEq]
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

  , f_withBoth ''Membership [MkEq, MkRead]

  , f ''OrganizationRequest
  , f ''OrganizationResponse
  , f ''OrganizationResponses
  , f ''OrganizationStatResponse
  , f ''OrganizationStatResponses

  , f_withBoth ''Param [MkEq, MkShow, MkQueryParam]
  , f_withBoth ''ParamTag [MkEq, MkShow, MkRead]
  , f_withBoth ''SortOrderBy [MkEq, MkShow, MkRead]
  , f_withBoth ''OrderBy [MkEq, MkShow, MkRead]

  , f_withBoth ''Permission [MkEq]
  , f ''Permissions

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
  , f_withBoth ''ProfileGender [MkEq, MkRead]
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
  , f_withBoth ''TyResourceType [MkEq]
  , f ''ResourceRequest
  , f ''ResourceResponse
  , f ''ResourceResponses

  , f ''ResourceStatResponse
  , f ''ResourceStatResponses

  , f_withBoth ''Size [MkEq]

  , f ''Splits
  , f_withBoth ''TySplits [MkEq]

  , f ''Substitutions
  , f_withBoth ''TySubstitutions [MkEq]

--  , f ''StyleResponse
--  , f ''StyleResponses
--
  , f ''StarRequest
  , f ''StarResponse
  , f ''StarResponses
  , f ''StarStatResponse
  , f ''StarStatResponses

  , f_withBoth ''SystemTeam [MkEq, MkRead]

  , f ''TeamRequest
  , f ''TeamResponse
  , f ''TeamResponses
  , f ''TeamStatResponse
  , f ''TeamStatResponses

  , f ''TeamMemberRequest
  , f ''TeamMemberResponse
  , f ''TeamMemberResponses
  , f ''TeamMemberStatResponse
  , f ''TeamMemberStatResponses

  , f ''TestRequest
  , f ''TestResponse
  , f ''TestResponses

  , f ''ThreadRequest
  , f ''ThreadResponse
  , f ''ThreadResponses

  , f ''ThreadStatResponse
  , f ''ThreadStatResponses

  , f_withBoth ''PostData [MkEq]

  , f ''ThreadPostRequest
  , f ''ThreadPostResponse
  , f ''ThreadPostResponses

  , f ''ThreadPostStatResponse
  , f ''ThreadPostStatResponses

  , f ''UserRequest
  , f ''UserResponse
  , f ''UserResponses
  , f ''UserSanitizedResponse
  , f ''UserSanitizedResponses
  , f ''UserSanitizedStatResponse
  , f ''UserSanitizedStatResponses

  , f_withBoth ''Visibility [MkEq, MkRead]

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

  , f ''TeamMemberPackResponse
  , f ''TeamMemberPackResponses

  , f ''UserPackResponse
  , f ''UserPackResponses

  , f ''UserSanitizedPackResponse
  , f ''UserSanitizedPackResponses

  , f ''GlobalGroupPackResponse
  , f ''GlobalGroupPackResponses

  , f ''GroupPackResponse
  , f ''GroupPackResponses

  , f ''GroupMemberPackResponse
  , f ''GroupMemberPackResponses

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

  , f ''PmInPackResponse
  , f ''PmInPackResponses

  , f ''PmOutPackResponse
  , f ''PmOutPackResponses
  ]



mkConvert
  (Options
    (defaultOptionsCleanPurescript "/tmp/LN.Convert.purs")
    (MkGHeader "import LN.T.Internal.Types\n" : (defaultPurescriptConvertMkGs "module LN.T.Internal.Convert where"))
    (defaultOptions_Haskell_adarqui "/tmp/LN.Convert.hs")
    (MkGHeader "import LN.T.Internal.Types\n" : (defaultHaskellConvertMkGs $ tplTestHeader "LN.T.Internal.Convert")))
  [ (''ApiRequest, ''ApiResponse)
  , (''ApiResponse, ''ApiRequest)

  , (''BoardRequest, ''BoardResponse)
  , (''BoardResponse, ''BoardRequest)

  , (''BucketRequest, ''BucketResponse)
  , (''BucketResponse, ''BucketRequest)

  , (''EmptyRequest, ''EmptyResponse)
  , (''EmptyResponse, ''EmptyRequest)

  , (''ForumRequest, ''ForumResponse)
  , (''ForumResponse, ''ForumRequest)

  , (''LeuronRequest, ''LeuronResponse)
  , (''LeuronResponse, ''LeuronRequest)

  , (''LeuronTrainingRequest, ''LeuronTrainingResponse)
  , (''LeuronTrainingResponse, ''LeuronTrainingRequest)

  , (''LikeRequest, ''LikeResponse)
  , (''LikeResponse, ''LikeRequest)

  , (''OrganizationRequest, ''OrganizationResponse)
  , (''OrganizationResponse, ''OrganizationRequest)

  , (''PmRequest, ''PmResponse)
  , (''PmResponse, ''PmRequest)

  , (''PmInRequest, ''PmInResponse)
  , (''PmInResponse, ''PmInRequest)

  , (''PmOutRequest, ''PmOutResponse)
  , (''PmOutResponse, ''PmOutRequest)

  , (''ProfileRequest, ''ProfileResponse)
  , (''ProfileResponse, ''ProfileRequest)

  , (''ReminderRequest, ''ReminderResponse)
  , (''ReminderResponse, ''ReminderRequest)

  , (''ReminderFolderRequest, ''ReminderFolderResponse)
  , (''ReminderFolderResponse, ''ReminderFolderRequest)

  , (''ResourceRequest, ''ResourceResponse)
  , (''ResourceResponse, ''ResourceRequest)

  , (''StarRequest, ''StarResponse)
  , (''StarResponse, ''StarRequest)

  , (''TeamRequest, ''TeamResponse)
  , (''TeamResponse, ''TeamRequest)

  , (''ThreadRequest, ''ThreadResponse)
  , (''ThreadResponse, ''ThreadRequest)

  , (''ThreadPostRequest, ''ThreadPostResponse)
  , (''ThreadPostResponse, ''ThreadPostRequest)

  , (''UserRequest, ''UserResponse)
  , (''UserResponse, ''UserRequest)

  , (''UserRequest, ''UserSanitizedResponse)
  , (''UserSanitizedResponse, ''UserRequest)
  ]



mkApi
  (Options
    ((defaultOptionsCleanPurescript "/tmp/LN.Api.purs") { debug = True })
    (MkGHeader "import LN.T.Internal.Types\n" : (defaultPurescriptApiMkGs "module LN.Api.Internal where"))
    ((defaultOptionsCleanHaskell "/tmp/LN.Api.hs" ) { debug = True })
    (MkGHeader haskellApiImports : (defaultHaskellApiMkGs $ tplTestHeader "LN.Api.Internal")))
  ''ApplicationError
  apiSpec_TH



mkApi
  (Options
    ((defaultOptionsCleanPurescript "/tmp/LN.Api.String.purs") { debug = True })
    (MkGHeader "import LN.T.Internal.Types\n" : (defaultPurescriptApiStringMkGs "module LN.Api.Internal.String where"))
    ((defaultOptionsCleanHaskell "/tmp/LN.Api.String.hs" ) { debug = True })
    (MkGHeader haskellApiImports : (defaultHaskellApiStringMkGs $ tplTestHeader "LN.Api.Internal.String")))
  ''ApplicationError
  apiSpec_String_TH
