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

This will make the `cowsay` binary available whilst you are inside the project
directory.

### Removing a package

```bash
nix-mate remove cowsay
```

This will remove the `cowsay` binary from your project directory.

### Searching for packages

```bash
nix-mate search nodejs
```

This will show a big list of packages matching that name that you can install.

### Versions of nixpkgs

`nix-mate` uses a pinned version of nixpkgs for installing so that we get the
same result every time.

#### View available versions

To view the available versions of nixpkgs run:

```bash
nix-mate tags
```

This will display all the available versions.

#### Use a specific version

You can choose to use a version using:

```bash
nix-mate set-version <version-number>
# for example
nix-mate set-version 19.03
```

This will update your project to use this new version.

#### Use most up to date version

If you'd just prefer to stay up to date, use:

```bash
nix-mate update
```

This will use the newest version.

#### Pinning to a custom nixpkgs commit

First, you will need to get the git hash for the commit you wish to use at
[nixpkgs](https://github.com/NixOS/nixpkgs/). Then run

```bash
nix-mate pin <commit-hash>

# for example
nix-mate pin 1f795f9f44607cc5bec70d1300150bfefcef2aae
```

## Who is this for?

This project is for users that would usually install project dependencies with `brew` or `apt-get` and would like a declarative way to do this per-project.

If you're already familiar with Nix-the-language, then this project isn't for you. If you're happy creating your own `shell.nix` files etc and want more control, then there are great tools like [lorri](https://github.com/target/lorri) out there for just this.

## Releasing a new version

```bash
git tag v0.0.1231233
git push origin --tags
```

Then, fill in the release info on the created page.
