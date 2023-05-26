# Static site generator

## Install

- Clone and navigate to repo, then run:

```sh
$ stack install .
```

## Usage

- Single file

```sh
$ ssghs convert -i example.md -o example.html
```

- Directory

```sh
$ ssghs conver-dir -i example/ -o dist/
```

Will convert each file in the example markdown directory to HTML and will also add an
index.html file.

## Without Installing(using stack)

```sh
$ stack exec ssghs -- convert-dir -i example -o dist
```
