[![Build Status](https://travis-ci.org/weecology/livedat.svg?branch=master)](https://travis-ci.org/weecology/livedat)
[![License](http://i.creativecommons.org/p/zero/1.0/88x31.png)](https://raw.githubusercontent.com/weecology/livedat/master/LICENSE)


#process:
[See https://github.com/dfalster/baad for examples]
0- Use secret folder to preprocess not publichable data.
1- Crear tesauro especies 2 archivos de R. ALFONSO COMPLETAR
1.1- tesaure crops.
2- Para cada estudio, lees, limpias y popular -> MASTER. 
3- Revision por autores. Datos - R - SPECIFIC QUESTIONS - decisions. NOTE ALL CHANGES.
4- Redo 1 and 2.
5- Create a MASTER insect sampling with all insect crop interactions.

#To Do:
Now:
[] Add process to this repo.
[] Add tests: e.g. lat long in country. Outlyers.
Next:
[] Analysis preliminar... analysis. 
[] Add Rader, Garibaldi, Kleijn... :hasta verano.
Future:
[] Data corrections by Authors: In Original.

# livedat

This is a **Template Repo** designed to assist in setting up a repository for regularly-updated data 
(new data are regularly added and need cleaning and curating). Read [our PLOS Biology paper](https://doi.org/10.1371/journal.pbio.3000125) for more details.

Instructions for creating an updating data workflow can be found at our companion website: 
## [UpdatingData.org](https://www.updatingdata.org/).

  The basic steps are:

    1. Clone the repository
    2. Configure the repository for your project
    3. Connect to Zenodo
    4. Connect to Travis
    5. Allow automated updating
    6. Add data code
    7. Add data
    8. Add data checks
    9. Update data

## Some Thoughts on Security

The Personal Access Token from GitHub provides full access to your entire account. It is equivalent to having your username and password. Travis and other CI companies are very serious about security, but there is always some risk when using credentials like this. To mitigate security issues you can set up a separate account purely for deploying your data and only give it access to the repositories that require it. Then use that account's Personal Access Token instead of the one for you primary account.