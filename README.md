# Format.Preserving.YAML

A format-preserving YAML parser.

## Progress Note

We are following the [YAML 1.2 Spec](http://yaml.org/spec/1.2/spec.html) in
general and use [Core Schema](http://yaml.org/spec/1.2/spec.html#id2804923) as
the reference for YAML native data structure in Haskell.

### Implementation

#### YAML Native Data Structure in Haskell

YAML supports multi-document content in one file. Each document could contains
Directive and Body (YAML Node).

```haskell
data YAML = YAML Documents

type Documents = [Document]

data Document = Document Directives Node
```

#### Finished Implementation

##### Directive

- Version Directive
- Tag Directive
- None-reserved YAML Directive

##### Scalar

- Comment
- Null
- Bool
- Int (base 16, base 10, base 8)
- Float including Inf and NaN
- Node Alias

##### Sequence

- Empty Sequence
- Single element Sequence
- Many elements Sequence

**TODO**: Investigate more tricky cases and add more tests.

#### Ongoing Implementation

##### Scalar

- Quoted Str (single, double)
- Node Anchor

##### Sequence

- Nested Sequence

**NOTE**: Fixing bugs. Implements parser for escaped character.

##### Miscellaneous

Constructing and researching about algorithm & data structure for block-scoped
indentation in YAML. This is the hardest part.

### Implementation Validation

I'm reading [Learn YAML in Y Minutes](https://learnxinyminutes.com/docs/yaml/)
to think about the implementation and looked at the YAML 1.2 Spec production
grammar for the best outcome. Making sure that the YAML is valid using online
[YAML Linter](http://www.yamllint.com/) and online
[YAML Parser](http://yaml-online-parser.appspot.com/). However, there are
several limitation in those YAML Linter and YAML Parser implementation. Hence,
we stick with the YAML 1.2 Spec to the end.

### Usage Example

Example will be added later. There are tests instead in `test/` directory.

## License

Licensed under [The BSD3 License](LICENSE).
