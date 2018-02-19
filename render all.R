### For merging the datasets ('merge' doesn't work because many rows
### don't have a partyPanelCode, which results in an expontentially
### huge dataset)
require('plyr');

### For clever path identification
require('here');

#rootPath <- "B:/Data/research/party panel";
sharedPath <- here::here();
rootPath <- file.path(sharedPath, "..");
subdirs <- grep("partypanel-\\d\\d.\\d$", list.files(rootPath), value=TRUE);
waves <- gsub("partypanel-", "", subdirs);
filenames <- paste0("partypanel-", waves,
                    "-render-report.R");
fullPaths <- file.path(rootPath,
                       subdirs,
                       "results - analysis scripts",
                       filenames);

### Set to TRUE when debugging
options(ufs.debug = FALSE);

### Create variable to keep track of Party Panel dataframes for merger
ppDataframes <- character();

### And of recruitment info
ppRecrInfo <- character();

### Run all party panel rendering scripts
lapply(fullPaths, source);

### Combine dataframes into one list of dataframes
dat <- lapply(ppDataframes, get);
names(dat) <- ppDataframes;

### Combine recruitment info into one list for later processing
ppRecrInfo <- sapply(ppRecrInfo, get,
                     simplify=FALSE,
                     USE.NAMES=TRUE);

### Clear memory of everything except the dataframes
allObjects <- ls();
allObjects <- allObjects[allObjects != 'dat'];
allObjects <- allObjects[allObjects != 'ppRecrInfo'];
rm(list=allObjects);
rm(allObjects);  ### Might as well be thorough

### Rename all columns
dat <- sapply(names(dat), function(dfName) {
  names(dat[[dfName]]) <-
    paste0(gsub("dat\\.pp", "t", dfName),
           "_",
           names(dat[[dfName]]));
  names(dat[[dfName]])[grep('partyPanelCode', names(dat[[dfName]]))] <-
    'partyPanelCode';
  return(dat[[dfName]]);
}, USE.NAMES=TRUE, simplify=FALSE);

if (length(dat) > 1) {
  ### We have more than one dataframe, so we can merge them.
  ### First, split each dataframe, into the rows where participants
  ### provided a partyPanelCode, and the rows where they didn't.
  ### Simply combine the last parts, and merge the first parts.

  nonMergableDat <- lapply(dat,
                           function(df) {
                             return(df[is.na(df$partyPanelCode), ]);
                           });
  mergableDat <- lapply(dat,
                        function(df) {
                          return(df[!is.na(df$partyPanelCode), ]);
                        });

  mergedDat <- mergableDat[[names(mergableDat)[1]]];
  for (currentDf in names(mergableDat)[-1]) {
    mergedDat <- join(x = mergedDat,
                      y = mergableDat[[currentDf]],
                      by = "partyPanelCode",
                      type = "full");
  }
  fullDat <- mergedDat;
  for (currentDf in names(nonMergableDat)) {
    fullDat <- rbind.fill(fullDat,
                          nonMergableDat[[currentDf]]);
  }
}

# sum(mergableDat$dat.pp15.1$partyPanelCode %in% mergableDat$dat.pp16.1$partyPanelCode);
# sum(mergableDat$dat.pp16.1$partyPanelCode %in% mergableDat$dat.pp17.1$partyPanelCode);
# sum(mergableDat$dat.pp17.1$partyPanelCode %in% mergableDat$dat.pp15.1$partyPanelCode);
