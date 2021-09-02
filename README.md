# Velle

Here's the repo for the new version of (mani)Velle, a tool supposed to be your coding buddy. Not in the AI-autocomplete way, rather by being a batteries-included toolkit meant to facilitate learning and using new technologies efficiently, as well as automating local and github scripts and files.

You'll be able to handle local and online repositories more easily, combining and loading templates for your projects. You'll have access to all the velle scripts written officially and by the community to make your workflow more comfortable and less repetitive.

Finally, with the (WIP) discover mode, you'll be able to learn about new technologies by seeing examples, and get the best tooling from the get-go.

## Installation

### Fresh Install
If you're new to velle, go grab the executable in [the releases tab](https://github.com/HiiGHoVuTi/haskvelle/releases), and execute the file from its folder.
Run this from the command-line:
```shell
./haskvelle install full
```
If all goes well, the shell should guide you through the installation. Otherwise, please report the issue either in [the issues tab](https://github.com/HiiGHoVuTi/haskvelle/issues), or by [contacting me](https://maximecan.ml).

### Updating

The logic is the same here, you can hop on [the releases tab](https://github.com/HiiGHoVuTi/haskvelle/releases), and grab the executable. However, you don't need to go through the installation procedure, you just need to move it to the same folder as the old executable, renaming it to `velle`. 
> Please note that your outdated velle might not have the proper `main.cfg` file, please grab the default one in this repository's `public-files` folder if you have any issue.

If you're on linux, you can also run:
```shell
velle install update
```

## Usage

```
> velle -h
Velle:

   run: runs a command from config
   watch: watches for events in the project
   load: loads a repo, either from local files, or from github

   local
      list: list all local repos
      save: saves the current folder as a local repo
      pull: pulls a repo from github <author>/<reponame>

   config
      init: initialize velle in cwd
      display-namespace: displays a single namespace in the config
      display: displays the whole config file as a list of key/value pairs
      display-global: displays a namespace from the global config

   install
      full: installs manivelle all at once
```

## Scripting

Velle is now scriptable using JavaScript ! When you type `run <command>` in Velle, it'll fetch `commands.<command>` in the config files in `.velle`. If one of the commands starts with `interp`, it'll run the corresponding javascript file. Velle runs a special subset of javascript based on Duktape. It doesn't support modern javascript sugar nor node modules, but has access to some unique functions, which will be documented soon. For a simple example, you can have a look at `public-files`, or this project's `.velle`. Yes, the manivelle developpers use Velle to make Velle !

## Watch mode
if your config looks like so:
```cfg
commands {
    on {
        changed =
            [ ["src", "interp compile.js"  ]
            , ["res", "interp updateRes.js"]
            ]
    }
}
```
You can run `velle watch` in the project's directory to have velle execute your scripts when certain events happen.
