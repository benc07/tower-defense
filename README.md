
# Team Name: tower-defense

## Team Members
- Pranav Kurungod Anoop, pk585
- Ben Chen, bc679
- Lydia Zhou, lz624
- Lydia Huang, lh729

## Setup

Requires OCaml and OPAM.

### Install Dependencies

```bash
opam install dune graphics ounit2
eval $(opam env)
```

For Ubuntu/WSL2, also run:

```bash
sudo apt update
sudo apt install libx11-dev
```

The GUI requires a graphical display (WSLg on Windows or XQuartz on macOS).

### Build and Run

From the project root:

```bash
dune build
dune test
dune exec ./bin/main.exe -- maps/demo.map
```
