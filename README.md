# fleetrace-delphi

This repository will contain a family of related Delphi projects side by side, so that you can compare them easily using a good merge tool. It is via the folder comparison view of that tool that you will be able to keep in control.

## FR69

FR69 is the first application I am going to upload, more will follow.

## How to build

The source code is for Delphi 10.2 (Tokyo) !

- I am using Community Edition.
- I am not using any third party components.
- It should work with Delphi 10.3 (Rio).

#### Configure the output dirs

This should be done immediately after opening a project for the first time!

Please go to project options and restore the output directories for exe and dcus to be the original default for a new project:

```
.\$(Platform)\$(Config)
```

Put this in both empty fields.

Delphi will recreate the .dproj locally from the dpr, because I have put the .dproj into gitignore,
but it does not restore the original-new-project-default value for the output dirs, that is why.

#### Configure the style to be used

Since the .dproj was automatically recreated, the style reference is missing from the generated .dproj file.

Please go to project options and select/check the Style (appearance) to be included,
Otherwise you will see an error window when the application starts.

At the bottom of the dpr file you can see a line that tries to set a style, e.g 'Silver'.
Include that style.

#### About res files

I do not want to include the normal project-name.res file (see gitignore),
so a new one will be recreated in your local repo when you compile.

I do include the other res files.
The resources for these will go into a separate directory together with the bat file to compile the rc files.



