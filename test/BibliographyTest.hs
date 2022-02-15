module BibliographyTest where
 
import Bibliography.Const.Structure
import Data.Text (pack)
import AssertUtils

testAllStructure = 
  [ testBuildTitleAndResponsibilityWithAllFields
  , testBuildIssueWithAllFields
  ]

testBuildTitleAndResponsibilityWithAllFields = assertEquals reference (build titleAndResponsibility)
  where
    titleAndResponsibility = TitleAndResponsibility (mainTitle, titleRelated, responsibilityList)
    mainTitle = MainTitle (pack "Moscow, A. A. Below social and economical")
    titleRelated = Just [TitleRelated (pack "conflict below projects")]
    responsibilityList = [Responsibility (pack "A. A. Moscow, A. A. Berend, A. U. Mosc")]
      
    reference = pack "Moscow, A. A. Below social and economical: conflict below projects / A. A. Moscow, A. A. Berend, A. U. Mosc"  
    
    
testBuildIssueWithAllFields = assertEquals reference (build issue) 
  where
    reference = pack "3-th iss. / additions L. N. Naum, rewrited with changes and additions"
    
    issueData = IssueData $ pack "3-th iss."
    
    issueRespons = IssueResponsibility $ pack "additions L. N. Naum"
    
    issueAdditional = IssueAdditional $ pack "rewrited with changes and additions"
    
    issue = Issue (issueData, Just [issueRespons], Just issueAdditional)
    
          