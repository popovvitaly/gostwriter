module Bibliography.Const.Structure where

import Data.Text (Text, pack, unpack)
import Data.List (intercalate)
import Bibliography.Const.Texts

-- | Интерфейс как областей, так и элементов описания
class Item a where
  label :: a -> Text
  descriptionMessage :: a -> Text
  photosIds :: a -> Maybe [Text]  

-- | Элемент описания
class (Item a) => Element a


-- | Основное заглавие (п. 5.2.3)
newtype MainTitle = MainTitle Text
instance Element MainTitle
instance Item MainTitle where
  label _ = mainTitleLabel
  descriptionMessage _ = mainTitleDescription
  photosIds _ = Nothing --todo add photos

-- | Сведения, относящиеся к заглавию (п. 5.2.5)
newtype TitleRelated = TitleRelated Text
instance Element TitleRelated
instance Item TitleRelated where
  label _ = titleRelatedLabel
  descriptionMessage _ = titleRelatedDescription
  photosIds _ = Nothing --todo add photos

-- | Сведения об ответственности (п. 5.2.6)
newtype Responsibility = Responsibility Text
instance Element Responsibility
instance Item Responsibility where
  label _ = responsibilityLabel
  descriptionMessage _ = responsibilityDescription
  photosIds _ = Nothing --todo add photos


-- | Сведения об издании (п. 5.3.3)
newtype IssueData = IssueData Text
instance Element IssueData
instance Item IssueData where
  label _ = issueDataLabel
  descriptionMessage _ = issueDataDescription
  photosIds _ = Nothing --todo add photos

-- | Первые сведения об ответственности, относящиеся к изданию (п. 5.3.5)
newtype IssueResponsibility = IssueResponsibility Text
instance Element IssueResponsibility
instance Item IssueResponsibility where
  label _ = issueDataResponsibilityLabel
  descriptionMessage _ = issueDataResponsibilityDescription
  photosIds _ = Nothing --todo add photos

-- | Дополнительные сведения об издании (п. 2.3.6)
newtype IssueAdditional = IssueAdditional Text
instance Element IssueAdditional
instance Item IssueAdditional where
  label _ = issueAdditionalLabel
  descriptionMessage _ = issueAdditionalDescription
  photosIds _ = Nothing --todo add photos
  

-- | Сведения о масштабе (п. 5.4.3.2)
newtype Scale = Scale Text
instance Element Scale
instance Item Scale where
  label _ = scaleLabel
  descriptionMessage _ = scaleDescription
  photosIds _ = Nothing -- todo add photos

-- | Сведения о проекции (п. 5.4.3.3)
newtype Projection = Projection Text
instance Element Projection
instance Item Projection where
  label _ = projectionLabel
  descriptionMessage _ = projectionDescription
  photosIds _ = Nothing --todo add photos

-- | Сведения о форме изложения нотного текста (для нотных ресурсов) (п. 5.4.4)
newtype FormData = FormData Text
instance Element FormData
instance Item FormData where
  label _ = formDataLabel
  descriptionMessage _ = formDataDescription
  photosIds _ = Nothing --todo add photos


-- | Место публикации, производства и/или распространения (п. 5.5.3)
newtype Place = Place Text
instance Element Place
instance Item Place where
  label _ = placeLabel
  descriptionMessage _ = placeDescription
  photosIds _ = Nothing --todo add photos

-- | Имя издателя, производителя и/или распространителя (п. 5.5.4)
newtype Publisher = Publisher Text
instance Element Publisher
instance Item Publisher where
  label _ = publisherLabel
  descriptionMessage _ = publisherDescription
  photosIds _ = Nothing --todo add photos

-- | Дата публикации, производства и/или распространения (п. 5.5.5)
newtype PublicationDate = PublicationDate Text
instance Element PublicationDate
instance Item PublicationDate where
  label _ = publicationDateLabel
  descriptionMessage _ = publicationDateDescription
  photosIds _ = Nothing --todo add photos


-- | Специфическое обозначение материала и объем
newtype SizeData = SizeData Text
instance Element SizeData
instance Item SizeData where
  label _ = sizeLabel
  descriptionMessage _ = sizeDescription
  photosIds _ = Nothing --todo add photos


-- | Основное заглавие серии/подсерии или многочастного монографического ресурса (п. 5.7.3)
newtype SeriesTitle = SeriesTitle Text
instance Element SeriesTitle
instance Item SeriesTitle where
  label _ = seriesTitleLabel
  descriptionMessage _ = seriesTitleDescription
  photosIds _ = Nothing --todo add photos

-- | Cведения об ответственности, относящиеся к серии/подсерии или многочастному монографическому ресурсу (п. 5.7.6)
newtype SeriesResponsibility = SeriesResponsibility Text
instance Element SeriesResponsibility
instance Item SeriesResponsibility where
  label _ = seriesResponsibilityLabel
  descriptionMessage _ = seriesResponsibilityDescription
  photosIds _ = Nothing --todo add photos

-- | Международный стандартный номер серии/подсерии или многочастного монографического ресурса (п. 5.7.7)
newtype Issn = Issn Text
instance Element Issn
instance Item Issn where
  label _ = issnLabel
  descriptionMessage _ = issnDescription
  photosIds _ = Nothing --todo add photos

-- | Номер выпуска серии/подсерии или многочастного монографического ресурса (п. 5.7.8)
newtype SeriesNumber = SeriesNumber Text
instance Element SeriesNumber
instance Item SeriesNumber where
  label _ = seriesNumberLabel
  descriptionMessage _ = seriesNumberDescription
  photosIds _ = Nothing --todo add photos                             


-- | Элемент примечания (п. 5.8)
newtype NoteElement = NoteElement Text
instance Element NoteElement
instance Item NoteElement where
    label _ = noteElementLabel
    descriptionMessage _ = noteElementDescription
    photosIds _ = Nothing --todo add photos    


-- | Идентификатор ресурса (п 5.9.3)
newtype ResourceIdentifier = ResourceIdentifier Text
instance Element ResourceIdentifier
instance Item ResourceIdentifier where
    label _ = resourceIdentifierLabel
    descriptionMessage _ = resourceIdentifierDescription
    photosIds _ = Nothing --todo add photos

-- | Фингерпринт (для старопечатных изданий) (п. 5.9.6)
newtype Fingerprint = Fingerprint Text
instance Element Fingerprint
instance Item Fingerprint where
    label _ = fingerprintLabel
    descriptionMessage _ = fingerprintDescription
    photosIds _ = Nothing --todo add photos    


-- | Вид содержания (п. 5.10.4)
newtype ContentType = ContentType Text
instance Element ContentType
instance Item ContentType where
    label _ = contentTypeLabel
    descriptionMessage _ = contentTypeDescription
    photosIds _ = Nothing --todo add photos

-- | Средство доступа (п. 5.10.8)
newtype AccessTool = AccessTool Text
instance Element AccessTool
instance Item AccessTool where 
    label _ = accessToolLabel
    descriptionMessage _ = accessToolDescription
    photosIds _ = Nothing --todo add photos    

class Build a where
  build :: a -> Text

-- | Область описания. (п. 4.3)
class (Item a) => Field a

-- | Область заглавия и сведений об ответственности. (п. 5.2)
newtype TitleAndResponsibility = TitleAndResponsibility
  ( MainTitle
  , Maybe [TitleRelated]
  , [Responsibility]  
  )
instance Field TitleAndResponsibility
instance Item TitleAndResponsibility where
  label _ = titleAndResponsibilityLabel
  descriptionMessage _ = titleAndResponsibilityDescriptionMessage
  photosIds _ = Nothing --todo add photos
instance Build TitleAndResponsibility where
  build (TitleAndResponsibility (MainTitle mainTitle, titleRelated, responsibility)) =
    mainTitle <> buildTitleRelated titleRelated <> pack " / " <> buildResponsibility responsibility
    where 
      buildTitleRelated :: Maybe [TitleRelated] -> Text
      buildTitleRelated Nothing = pack ""
      buildTitleRelated (Just list) = foldl (\acc (TitleRelated x) -> acc <> x) (pack ": ") list
      
      buildResponsibility :: [Responsibility] -> Text
      buildResponsibility [] = pack ""
      buildResponsibility [Responsibility x] = x
      buildResponsibility ((Responsibility x):xs) = x <> pack "; " <> buildResponsibility xs

-- | Область издания
newtype Issue = Issue
  ( IssueData
  , Maybe [IssueResponsibility]
  , Maybe IssueAdditional
  )
instance Field Issue  
instance Item Issue where
  label _ = issueLabel
  descriptionMessage _ = issueDescriptionMessage
  photosIds _ = Nothing --todo add photos
instance Build Issue where 
  build (Issue (IssueData issueData, issueRespons, issueAdditional)) = 
    issueData <> buildIssueRespons issueRespons <> buildIssueAdditional issueAdditional
    where       
      buildIssueAdditional :: Maybe IssueAdditional -> Text
      buildIssueAdditional Nothing = pack ""
      buildIssueAdditional (Just (IssueAdditional additional)) = pack ", " <> additional
      
      buildIssueRespons :: Maybe [IssueResponsibility] -> Text
      buildIssueRespons Nothing = pack ""
      buildIssueRespons (Just respons) = pack " / " <> buildIssueRespons' respons
        where
        buildIssueRespons' [] = pack ""
        buildIssueRespons' [IssueResponsibility x] = x
        buildIssueRespons' ((IssueResponsibility x):xs) = x <> pack "; " <> buildIssueRespons' xs  

-- | Специфическая область материала или вида ресурса
newtype Specific = Specific
  ( Maybe Scale
  , Maybe Projection
  , Maybe FormData 
  )
instance Field Specific
instance Item Specific where
  label _ = specificLabel
  descriptionMessage _ = specificDescriptionMessage
  photosIds _ = Nothing -- todo add photos
instance Build Specific where
  build (Specific (Nothing, Nothing, Just (FormData formData))) = formData
  build (Specific (scale, projection, Nothing)) = buildMap scale projection
    where
        buildMap :: Maybe Scale -> Maybe Projection -> Text
        buildMap (Just (Scale scale')) (Just (Projection projection')) = scale' <> pack "; " <> projection'
        buildMap (Just (Scale scale')) Nothing = scale' 
        buildMap Nothing _ = pack ""
  build _ = pack "Что-то странное! И карта и ноты?"

-- | Область публикации, производства, распространения и т. д.
newtype PublicationProductionDistribution = PublicationProductionDistribution
  ( [Place]
  , Publisher
  , PublicationDate  
  )
instance Field PublicationProductionDistribution
instance Item PublicationProductionDistribution where
  label _ = ppdLabel
  descriptionMessage _ = ppdDescriptionMessage
  photosIds _ = Nothing -- todo add photos
instance Build PublicationProductionDistribution where
  build (PublicationProductionDistribution (places, Publisher publisher, PublicationDate pubDate)) = 
    buildPlaces places <> pack ": " <> publisher <> pack ", " <> pubDate
    where
      buildPlaces :: [Place] -> Text
      buildPlaces = foldl (\acc (Place p) -> acc <> pack "; " <> p) (pack "") 

-- | Область физической характеристики
newtype Physical = Physical SizeData
instance Field Physical
instance Item Physical where
  label _ = physicalLabel
  descriptionMessage _ = physicalDescriptionMessage
  photosIds _ = Nothing --todo add photos
instance Build Physical where
  build (Physical (SizeData sizeData)) = sizeData  

-- | Область серии и многочастного монографического ресурса
newtype Serial = Serial
  ( SeriesTitle
  , Maybe [SeriesResponsibility]
  , Issn
  , SeriesNumber
  )
instance Field Serial
instance Item Serial where
  label _ = serialLabel
  descriptionMessage _ = serialDescriptionMessage
  photosIds _ = Nothing --todo add photos
instance Build Serial where
  build (Serial (SeriesTitle seriesTitle, seriesResp, Issn issn, SeriesNumber seriesNumber)) = 
    pack "(" <> seriesTitle <> buildResp seriesResp <> pack "; " <> issn <> pack "; " <> seriesNumber <> pack ")"
      where 
        buildResp :: Maybe [SeriesResponsibility] -> Text
        buildResp Nothing = pack ""
        buildResp (Just respList) = pack " / " <> buildRespList respList
          where 
            buildRespList :: [SeriesResponsibility] -> Text
            buildRespList respList' = pack $ intercalate ", " $ map unpack $ foldl (\acc (SeriesResponsibility r) -> acc ++ [r]) [] respList'
             
        
-- | Область примечания
newtype Note = Note [NoteElement]
instance Field Note
instance Item Note where
  label _ = noteLabel
  descriptionMessage _ = noteDescriptionMessage
  photosIds _ = Nothing --todo add photos  
instance Build Note where
  build (Note elementsList) = pack $ intercalate ", " $ map unpack $ foldl (\acc (NoteElement r) -> acc ++ [r]) [] elementsList

-- | Область идентификатора ресурса и условий доступности
newtype IdentifierAccessibility = IdentifierAccessibility
  ( Maybe [ResourceIdentifier]
  , Maybe Fingerprint  
  )
instance Field IdentifierAccessibility
instance Item IdentifierAccessibility where
  label _ = idAccLabel
  descriptionMessage _ = idAccDescriptionMessage
  photosIds _ = Nothing --todo add photos
instance Build IdentifierAccessibility where
  build (IdentifierAccessibility (Just ids, Nothing)) = pack $ intercalate ", " $ map unpack $ foldl (\acc (ResourceIdentifier r) -> acc ++ [r]) [] ids
  build (IdentifierAccessibility (Nothing, Just (Fingerprint fp))) = fp
  build _ = pack ""

-- | Область вида содержания и средства доступа
newtype ContentAndsAccess = ContentAndsAccess
  ( Maybe ContentType
  , Maybe AccessTool
  )
instance Field ContentAndsAccess
instance Item ContentAndsAccess where
  label _ = contentLabel
  descriptionMessage _ = contentDescriptionMessage
  photosIds _ = Nothing
instance Build ContentAndsAccess where
  build (ContentAndsAccess (contentType, accessTool)) = ct <> if null' ct then pack "" else pack ": " <> at
    where 
      buildContentType :: Maybe ContentType -> Text
      buildContentType (Just (ContentType ct')) = ct'
      buildContentType Nothing = pack "" 
      ct = buildContentType contentType
      
      buildAccessTool :: Maybe AccessTool -> Text
      buildAccessTool (Just (AccessTool at')) = at'
      buildAccessTool Nothing = pack ""
      at = buildAccessTool accessTool


-- | Одна запись библиографического списка
newtype Record = Record
  ( TitleAndResponsibility
  , Maybe Issue
  , Maybe Specific
  , PublicationProductionDistribution
  , Maybe Physical -- todo check maybe-ness?
  , Maybe Serial
  , Maybe Note
  , IdentifierAccessibility 
  , Maybe ContentAndsAccess
  )
  
instance Build Record where
  build (Record (titleAndResp, issue, specific, ppd, physical, serial, note, identAndAcc, contentAndAcc)) = 
    build titleAndResp <> builtIssue <> builtSpecific <> builtPpd <> builtPhysical <> builtSerial <> builtNote <> builtIdA <> builtIdContACc
      where
        prefix :: Text -> Text
        prefix t = if null' t then t else pack ".– " <> t
        
        builtIssue = prefix . build' $ issue
          where
            build' :: Maybe Issue -> Text
            build' (Just field) = build field
            build' Nothing = pack ""
            
        builtSpecific = prefix . build' $ specific 
          where 
            build' :: Maybe Specific -> Text
            build' (Just field) = build field
            build' Nothing = pack ""
            
        builtPpd = prefix . build $ ppd    
        
        builtPhysical = prefix . build' $ physical
          where 
            build' :: Maybe Physical -> Text
            build' (Just field) = build field
            build' Nothing = pack ""
            
        builtSerial = prefix . build' $ serial
          where
            build' :: Maybe Serial -> Text
            build' (Just field) = build field
            build' Nothing = pack ""   
            
        builtNote = prefix . build' $ note
           where
            build' :: Maybe Note -> Text
            build' (Just field) = build field
            build' Nothing = pack ""    
            
        builtIdA = prefix . build $ identAndAcc
        
        builtIdContACc = prefix . build' $ contentAndAcc
          where
            build' :: Maybe ContentAndsAccess -> Text
            build' (Just field) = build field
            build' Nothing = pack ""                
                       
null' :: Text -> Bool
null' = null . unpack