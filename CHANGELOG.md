# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.8.0] - 2024-2-15

### Added

- Compiler and VM with async support and plug-ins.
- Static method call syntax for method calls on enums, e.g. `MyType::my_method(instance, arg)` vs `instance.my_method(arg)`. What's this really called? Associated function?
- Missing Function Definition Error
- `else if` support
- Literal pattern match expressions
- Added ValueType to Value enum
- Variable patten match expressions
- Unit Enum patten match expressions
- Tuple Enum patten match expressions
- Async HttpClient test
- `[]::push()` support

### Fixed

- Undefined function are now caught at extrusion time
- Now importing functions from external dwarf files in the REPL
- Proxy objects from plugins are now properly looked up in the LuDog store
- `use` statements now track scopes, and properly resolve references
- Generics. Just. Work. 💯. (Famous last words)
- Plugin-in API for writing plug-ins in dwarf is awesome! 💥
- Async dwarf interpreter tests are running now
- Generated .ore files now `use` imported objects

### Changed

- Improved module support
- Contexts contain sarzak references
- Improved match expression handling.
- VM API drastically improved
- The full path to a type, e.g., `::std::option::Option` is now stored in the LuDog store as the type's name.
- Despite the performance hit in the VM, discussed below, the looping benchmark increased by 7%. This is most likely due to creating the StackValue enum to keep as many pointers off the stack as possible.

### Busted

- 10% speed regression in VM, most likely suspect is an increase in the instruction size for lambdas, as well as async I'm sure. This can easily be addressed, however now is not the time. This feels like something to do when the Value that the VM is using is replaced with something more streamlined.
- 10% speed regression by managing our oun call stack and not using recursion in the VM


### Todo

- [ ] VM improvements
  - [ ] Create specialized Value type for VM
  - [ ] Streamline instruction size
  - [ ] Re-implement the VM run loop to utilize recursion instead of managing the stack manually -- it's faster.
- [ ] Compiler improvements
  - [ ] Keyhole optimizer
  - [ ] Implement string::format -- this is going to require parsing expressions in the strings, and dealing with it in the extruder/compiler/interpreter. At the same time I should change the way that format strings work. In fact, I may introduce `` strings, that don't require the `"".format()"` syntax.



## [0.7.0] - 2023-12-16

### Added

- This file.
It's long overdue that I maintain a changelog.

[unreleased]: https://github.com/uberfoo/dwarf/compare/v0.8.0...feature/compiler
[0.8.0]: https://github.com/uberFoo/dwarf/releases/tag/v0.8.0
[0.7.0]: https://github.com/uberFoo/dwarf/releases/tag/v0.7.0