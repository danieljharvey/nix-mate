# nix-mate

Install things! Delete them again! Install them again! Delete them again! It's up to you!

Nix Mate is a (very much WIP) terminal front end to the Nix Package Manager. It allows clean installation of per-project dependencies.

## Installation

This is quite manual at the moment. You will need:

Nix - https://nixos.org/nix/

Haskell Stack - https://docs.haskellstack.org/en/stable/README/

Direnv - https://direnv.net/

```bash
git clone https://github.com/danieljharvey/nix-mate

stack install
```

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

You will then need to run `direnv reload` or exit and re-enter the folder to put the new package in your path.

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
