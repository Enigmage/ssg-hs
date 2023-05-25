# Static site generator

## Using stack

```sh
$ stack exec ssghs -- convert-dir -i example -o dist                                                              dev/ssg-hs main
```

Will convert each file in the example markdown directory to HTML and will also add an
index.html file.
