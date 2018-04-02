# aipObstacleImporter
This program converts the aip pdf documents containing obstacles (ENR 5.4 usually).

each branch is for a different format (AIP), e.g. DFS, Austrocontrol, ...

## parameter:
-dir ... the directory where the pdfs are located, the program will loop them through. 
         important is that the wording of the file contains ENR_5_4 and .pdf.
         
         
## DFS Branch specific 
no parameter is needed, but folder in root called "aipDocs", with a list of all pdf docs needed, e.g.
- ED_ENR_5_4_Bayern_en_2018-03-29.pdf,
- ED_ENR_5_4_Berlin_en_2017-04-27.pdf,
- ...

The docs are organized in states. 

# Output
a euronav featurecode file will be generated, needed for conversion in the **directors /out** which is in root in addition to the shapefiles created with dotSpatial libs.

