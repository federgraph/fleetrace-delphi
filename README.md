# fleetrace-delphi

This repository will contain a family of related Delphi projects side by side, so that you can compare them easily using a good merge tool. It is via the folder comparison view that tool that you will be able to manage the projects.

## FR69

FR69 is the first application I am going to upload, more will follow.

## How to build

The sourced code is for Delphi 10.2 (Tokyo) !

- I am using Community Edition.
- I am not using any third party components.
- It should work with Delhi 10.3 (Rio).

#### First action after opening project for the first time

Please go to project options and restore the output dir for exe and dcu to be the original default for a new project to:

```
.\$(Platform)\$(Config)
```

I have put the .dproj into gitignore, Delphi will recreated it locally from the dpr. But it does not restore the original new project default for the output dirs.

#### About res files

I do not want to include the normal project-name.res file, so a new one will be recreated in your local repo.

- But I do include other res files,
- the resources for these res files will go into a separate directory
- together with the bat file to compile the rc file.



