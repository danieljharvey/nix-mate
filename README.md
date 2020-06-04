# nix-mate

Install things! Delete them again! Install them again! Delete them again! It's up to you!

Nix Mate is a (very much WIP) terminal front end to the [Nix Package Manager](https://nixos.org/). It allows clean installation of per-project dependencies.

## Installation

You will need the following installed:

Nix - [install instructions](https://nixos.org/download.html)

Direnv - [install instructions](https://direnv.net/docs/installation.html) 

### MacOS

Easy!

```bash
brew update && brew install danieljharvey/tools/nix-mate
```

### Ubuntu

Currently no nice packaging available, but binaries can be downloaded from the
[releases](https://github.com/danieljharvey/nix-mate/releases) page.

## Getting started

### Set up a nix-mate project

```bash
mkdir my-excellent-project
cd my-excellent-project
nix-mate init
```

This will setup a new project in this folder. Direnv will probably ask you to run `direnv allow` at this point, so go for it.

### Adding a package

```bash
nix-mate add cowsay
```

### Removing a package

```bash
nix-mate remove cowsay
```

### Searching for packages

(The output needs formatting, bare with me)

```bash
nix-mate search nodejs
```

This will show a big list of packages matching that name that you can install.

## Who is this for?

This project is for users that would usually install project dependencies with `brew` or `apt-get` and would like a declarative way to do this per-project.

If you're already familiar with Nix-the-language, then this project isn't for you. If you're happy creating your own `shell.nix` files etc and want more control, then there are great tools like [lorri](https://github.com/target/lorri) out there for just this.
