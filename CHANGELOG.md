# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Compiler and VM
- Static method call syntax for method calls on enums. What's this really called? Associated function?
- Missing Function Definition Error
- `else if` support
- Literal pattern match expressions
- Added ValueType to Value enum

### Fixed

- Undefined function are now caught at extrusion time
- Now importing functions from external dwarf files in the REPL
- Proxy objects from plugins are now properly looked up in the LuDog store
- `use` statements now track scopes, and properly resolve references

### Changed

- Improved module support
- Contexts contain sarzak references
- Improved match expression handling.

## [0.0.7] - 2023-12-16

### Added

- This file.
It's long overdue that I maintain a changelog.

[unreleased]: https://github.com/uberfoo/dwarf/compare/v0.0.7...feature/compiler
[0.0.7]: https://github.com/uberFoo/dwarf/releases/tag/v0.0.7