<p align="center">Work In Progress</p>

<br />

# Klassco
Just as components helps developpers abstract and avoid repetition,
**Klassco** does the same to your css classnames inside your components.


<br />

## How Does Klassco Work?
There are three available modes, each independent of the others. This means
that if you're only interested in reducing the bundle size, you can use
Klassco solely for that purpose.

1. **Build**: It optimizes the build size by removing all duplicate combos.

2. **Dev**: It scans all of your HTML files, JavaScript components, and other
   templates for classname combo duplications to help you reuse styles and
   create higher abstractions.

3. **Watch**: It automatically replaces classname combinations with new
   utilities as you type, using the ones you have created or loaded.


<br />

Klassco uses mathematical combinations to thoroughly explore and analyze
**every imaginable combination** of your CSS classnames, to help you identify
subtle patterns, spot redundancies, and create a carefully curated selection
of higher level utilities **— designed to save you time and effort.**


<br />

## Documentation
For full documentation, visit [klassco.cipherlogs.com](https://klassco.cipherlogs.com)


<br />

## Installation
The simplest and fastest way is to use npm

```bash
$ npm install -g klassco
```

<br />

If you want to compile from source, you’ll need to have a Haskell compiler
installed (or stack). Once you have the compiler, you can run the following
commands to build and execute the program

```
$ stack build
$ stack exec Klassco-exe
```

<br />

## Contribution
If you're passionate about clarity and want to make the project more
accessible to everyone, contributing to the documentation is a fantastic way
to get involved.

See the [CONTRIBUTING](./CONTRIBUTING.md) file for details.


<br />

## License
This project is licensed under the GPL-3.0 License - see the
[LICENSE](./LICENSE) file for details.
