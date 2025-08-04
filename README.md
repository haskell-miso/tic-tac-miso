:ramen: ❌ ⭕ tic-tac-miso
=============================

A simple tic-tac-toe implemention.

Play it live [here](https://tic-tac-miso.haskell-miso.org).

> [!TIP]
> This requires installing [nix](https://nixos.org) with [Nix Flakes](https://wiki.nixos.org/wiki/Flakes) enabled.
> Although not required, we recommend using [miso's binary cache](https://github.com/dmjio/miso?tab=readme-ov-file#binary-cache).

### Development

Call `nix develop` to enter a shell with [GHC 9.12.2](https://haskell.org/ghc)

```bash
$ nix develop
```

Once in the shell, you can call `cabal run` to start the development server and view the application at http://localhost:8080

### Build (Web Assembly)

```bash
$ nix develop .#wasm --command bash -c "make"
```

### Build (JavaScript)

```bash
$ nix develop .#ghcjs --command bash -c "build"
```

### Serve

To host the built application you can call `serve`

```bash
$ nix develop .#wasm --command bash -c "serve"
```

### Clean

```bash
$ nix develop .#wasm --command bash -c "make clean"
```

This comes with a GitHub action that builds and auto hosts the example.
