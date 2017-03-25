{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib (
) where



import           Haskell.Interop.Prime
import           Prelude

import           LN.T
import           LN.T.Job

import           Lib2



f_mkType'
  ["LN.T.Permission"]
  "LN.T.ACL" "LN/T/ACL" $
  [ f_withBoth ''ACL [MkEq] ]



f_mkType "LN.T.Api" "LN/T/Api" $
  [ f ''ApiRequest
  , f ''ApiResponse
  , f ''ApiResponses
  ]



f_mkType "LN.T.Board" "LN/T/Board" $
  [ f ''BoardRequest
  , f ''BoardResponse
  , f ''BoardResponses
  , f ''BoardStatResponse
  , f ''BoardStatResponses
  ]



f_mkType "LN.T.Bucket" "LN/T/Bucket" $
  [ f ''BucketRequest
  , f ''BucketResponse
  , f ''BucketResponses
  ]



f_mkType "LN.T.Count" "LN/T/Count" $
  [ f ''CountResponse
  , f ''CountResponses
  ]



f_mkType "LN.T.DepList" "LN/T/DepList" $
  [ f ''DepList ]



f_mkType "LN.T.Ent" "LN/T/Ent" $
  [ f_withBoth ''Ent [MkEq, MkRead] ]



f_mkType "LN.T.Error" "LN/T/Error" $
  [ f_withBoth ''ApplicationError [MkEq, MkDefault "Error_Unknown"]
  , f_withBoth ''ValidationError [MkEq, MkDefault "Validate Validate_Unknown Nothing"]
  , f_withBoth ''ValidationErrorCode [MkEq, MkDefault "Validate_Unknown"]
  ]



f_mkType'
  ["LN.T.Visibility"]
  "LN.T.Forum" "LN/T/Forum" $
  [ f ''ForumRequest
  , f ''ForumResponse
  , f ''ForumResponses
  , f ''ForumStatResponse
  , f ''ForumStatResponses
  ]



f_mkType'
  ["LN.T.Membership", "LN.T.Visibility"]
  "LN.T.GlobalGroup" "LN/T/GlobalGroup" $
  [ f ''GlobalGroupRequest
  , f ''GlobalGroupResponse
  , f ''GlobalGroupResponses
  , f ''GlobalGroupStatResponse
  , f ''GlobalGroupStatResponses
  ]



f_mkType
  "LN.T.Group" "LN/T/Group" $
  [ f ''GroupRequest
  , f ''GroupResponse
  , f ''GroupResponses
  , f ''GroupStatResponse
  , f ''GroupStatResponses
  ]



f_mkType "LN.T.GroupMember" "LN/T/GroupMember" $
  [ f ''GroupMemberRequest
  , f ''GroupMemberResponse
  , f ''GroupMemberResponses
  , f ''GroupMemberStatResponse
  , f ''GroupMemberStatResponses
  ]



f_mkType'
  ["LN.T"]
  "LN.T.Job" "LN/T/Job" $
  [ f_withBoth ''Job [MkEq]
  , f_withBoth ''Queue [MkEq]
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
  ]



f_mkType "LN.T.LeuronTraining" "LN/T/LeuronTraining" $
  [ f_withBoth ''LeuronTrainingSummary [MkEq, MkRead]
  , f ''LeuronTrainingRequest
  , f ''LeuronTrainingResponse
  , f ''LeuronTrainingResponses
  , f ''LeuronTrainingStatResponse
  , f ''LeuronTrainingStatResponses
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



f_mkType "LN.T.Membership" "LN/T/Membership" $
  [ f_withBoth ''Membership [MkEq, MkRead] ]




f_mkType'
  ["LN.T.Membership", "LN.T.Visibility"]
  "LN.T.Organization" "LN/T/Organization" $
  [ f ''OrganizationRequest
  , f ''OrganizationResponse
  , f ''OrganizationResponses
  , f ''OrganizationStatResponse
  , f ''OrganizationStatResponses
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



f_mkType "LN.T.Pm" "LN/T/Pm" $
  [ f ''PmRequest
  , f ''PmResponse
  , f ''PmResponses
  ]



f_mkType "LN.T.PmIn" "LN/T/PmIn" $
  [ f ''PmInRequest
  , f ''PmInResponse
  , f ''PmInResponses
  ]



f_mkType "LN.T.PmOut" "LN/T/PmOut" $
  [ f ''PmOutRequest
  , f ''PmOutResponse
  , f ''PmOutResponses
  ]



f_mkType' ["LN.T.Ent"] "LN.T.Profile" "LN/T/Profile" $
  [ f ''ProfileX
  , f_withBoth ''ProfileGender [MkEq, MkRead]
  , f ''ProfileRequest
  , f ''ProfileResponse
  , f ''ProfileResponses
  ]



f_mkType' ["LN.T.Visibility"] "LN.T.Reminder" "LN/T/Reminder" $
  [ f ''ReminderRequest
  , f ''ReminderResponse
  , f ''ReminderResponses
  , f ''ReminderFolderRequest
  , f ''ReminderFolderResponse
  , f ''ReminderFolderResponses
  ]



f_mkType' ["LN.T.DepList", "LN.T.Visibility"] "LN.T.Resource" "LN/T/Resource" $
  [ f ''ResourceType
  , f_withBoth ''TyResourceType [MkEq]
  , f ''ResourceRequest
  , f ''ResourceResponse
  , f ''ResourceResponses
  , f ''ResourceStatResponse
  , f ''ResourceStatResponses
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



f_mkType' ["LN.T.Visibility", "LN.T.Membership"] "LN.T.Team" "LN/T/Team" $
  [ f_withBoth ''SystemTeam [MkEq, MkRead]
  , f ''TeamRequest
  , f ''TeamResponse
  , f ''TeamResponses
  , f ''TeamStatResponse
  , f ''TeamStatResponses
  ]



f_mkType "LN.T.TeamMember" "LN/T/TeamMember" $
  [ f ''TeamMemberRequest
  , f ''TeamMemberResponse
  , f ''TeamMemberResponses
  , f ''TeamMemberStatResponse
  , f ''TeamMemberStatResponses
  ]



f_mkType "LN.T.Thread" "LN/T/Thread" $
  [ f ''ThreadRequest
  , f ''ThreadResponse
  , f ''ThreadResponses
  , f ''ThreadStatResponse
  , f ''ThreadStatResponses
  ]



f_mkType "LN.T.ThreadPost" "LN/T/ThreadPost" $
  [ f_withBoth ''PostData [MkEq]
  , f ''ThreadPostRequest
  , f ''ThreadPostResponse
  , f ''ThreadPostResponses
  , f ''ThreadPostStatResponse
  , f ''ThreadPostStatResponses
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
  [ f_withBoth ''Visibility [MkEq, MkRead] ]



f_mkType'
  ["LN.T.Ent"]
  "LN.T.View" "LN/T/View" $
  [ f ''ViewRequest
  , f ''ViewResponse
  , f ''ViewResponses
  ]



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



--
-- Packs
--



f_mkType'
  ["LN.T.Organization", "LN.T.User", "LN.T.Team", "LN.T.Like", "LN.T.Star", "LN.T.Permission"]
  "LN.T.Pack.Organization" "LN/T/Pack/Organization" $
  [ f ''OrganizationPackResponse
  , f ''OrganizationPackResponses
  ]



f_mkType'
  ["LN.T.Team", "LN.T.User", "LN.T.Permission"]
  "LN.T.Pack.Team" "LN/T/Pack/Team" $
  [ f ''TeamPackResponse
  , f ''TeamPackResponses
  ]



f_mkType'
  ["LN.T.TeamMember", "LN.T.User", "LN.T.Permission"]
  "LN.T.Pack.TeamMember" "LN/T/Pack/TeamMember" $
  [ f ''TeamMemberPackResponse
  , f ''TeamMemberPackResponses
  ]



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
  ["LN.T.GlobalGroup", "LN.T.User", "LN.T.Permission"]
  "LN.T.Pack.GlobalGroup" "LN/T/Pack/GlobalGroup" $
  [ f ''GlobalGroupPackResponse
  , f ''GlobalGroupPackResponses
  ]



f_mkType'
  ["LN.T.Group", "LN.T.User", "LN.T.Permission", "LN.T.Organization"]
  "LN.T.Pack.Group" "LN/T/Pack/Group" $
  [ f ''GroupPackResponse
  , f ''GroupPackResponses
  ]



f_mkType'
  ["LN.T.GroupMember", "LN.T.User"]
  "LN.T.Pack.GroupMember" "LN/T/Pack/GroupMember" $
  [ f ''GroupMemberPackResponse
  , f ''GroupMemberPackResponses
  ]



f_mkType'
  ["LN.T.Forum", "LN.T.User", "LN.T.Permission", "LN.T.Organization", "LN.T.Star", "LN.T.Like"]
  "LN.T.Pack.Forum" "LN/T/Pack/Forum" $
  [ f ''ForumPackResponse
  , f ''ForumPackResponses
  ]



f_mkType'
  ["LN.T.Permission", "LN.T.Organization", "LN.T.User", "LN.T.Forum", "LN.T.Board", "LN.T.Thread", "LN.T.ThreadPost", "LN.T.Like", "LN.T.Star"]
  "LN.T.Pack.Board" "LN/T/Pack/Board" $
  [ f ''BoardPackResponse
  , f ''BoardPackResponses
  ]



f_mkType'
  ["LN.T.Permission", "LN.T.Organization", "LN.T.User", "LN.T.Forum", "LN.T.Board", "LN.T.Thread", "LN.T.ThreadPost", "LN.T.Like", "LN.T.Star"]
  "LN.T.Pack.Thread" "LN/T/Pack/Thread" $
  [ f ''ThreadPackResponse
  , f ''ThreadPackResponses
  ]



f_mkType'
  ["LN.T.Permission", "LN.T.Organization", "LN.T.User", "LN.T.Forum", "LN.T.Board", "LN.T.Thread", "LN.T.ThreadPost", "LN.T.Like", "LN.T.Star"]
  "LN.T.Pack.ThreadPost" "LN/T/Pack/ThreadPost" $
  [ f ''ThreadPostPackResponse
  , f ''ThreadPostPackResponses
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
  ["LN.T.PmIn", "LN.T.User"]
  "LN.T.Pack.PmIn" "LN/T/Pack/PmIn" $
  [ f ''PmInPackResponse
  , f ''PmInPackResponses
  ]



f_mkType'
  ["LN.T.PmOut", "LN.T.User"]
  "LN.T.Pack.PmOut" "LN/T/Pack/PmOut" $
  [ f ''PmOutPackResponse
  , f ''PmOutPackResponses
  ]



f_mkType'
  ["LN.T.Resource", "LN.T.Leuron", "LN.T.LeuronTraining", "LN.T.Bucket"]
  "LN.T.Templates" "LN/T/Templates" $
  [ f ''Templates ]



mkConvert
  (Options
    (defaultOptionsCleanPurescript "../purescript-ln-types/src/LN/T/Convert.purs")
    (MkGHeader "import LN.T\n" : (defaultPurescriptConvertMkGs "module LN.T.Convert where"))
    (defaultOptions_Haskell_adarqui "../haskell-ln-types/src/LN/T/Convert.hs")
    (MkGHeader "import LN.T\n" : (defaultHaskellConvertMkGs $ tplTestHeader "LN.T.Convert")))
  [ (''ApiRequest, ''ApiResponse)
  , (''ApiResponse, ''ApiRequest)

  , (''BoardRequest, ''BoardResponse)
  , (''BoardResponse, ''BoardRequest)

  , (''BucketRequest, ''BucketResponse)
  , (''BucketResponse, ''BucketRequest)

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
    ((defaultOptionsCleanPurescript "../purescript-ln-api/src/LN/Api.purs") { debug = True })
    (MkGHeader "import LN.T\n" : (defaultPurescriptApiMkGs "module LN.Api where"))
    ((defaultOptionsCleanHaskell "../ln-api/src/LN/Api.hs" ) { debug = True })
    (MkGHeader haskellApiImports : (defaultHaskellApiMkGs $ tplTestHeader "LN.Api")))
  ''ApplicationError
  apiSpec_TH



mkApi
  (Options
    ((defaultOptionsCleanPurescript "../purescript-ln-api/src/LN/Api/String.purs") { debug = True })
    (MkGHeader "import LN.T\n" : (defaultPurescriptApiStringMkGs "module LN.Api.String where"))
    ((defaultOptionsCleanHaskell "../ln-api/src/LN/Api/String.hs" ) { debug = True })
    (MkGHeader haskellApiImports : (defaultHaskellApiStringMkGs $ tplTestHeader "LN.Api.String")))
  ''ApplicationError
  apiSpec_String_TH
