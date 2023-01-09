# SPS

Scheme library to specify network protocol state machines for specification-based symbolic execution of stateful network protocol implementations.
This Scheme library is intended to be used with [a fork][symex-vp sps] of [SymEx-VP][symex-vp github].
More details about this library are provided in the IEEE Internet of Things Journal publication “Specification-based Symbolic Execution for Stateful Network Protocol Implementations in the IoT”.

## Installation

This library is intended to be used with [CHICKEN Scheme][chicken web].
It depends on the [sisl][sisl github] library which needs to be installed separately manually.
If both sisl and CHICKEN are installed and configured corretly then the SPS library can be installed using:

    $ chicken-install

## Usage Example

Example network protocol specifications are provided in the `./example` directory.
More details are also provided in the aforementioned journal publication.

## License

This library is licensed under MIT.
The `lib/util.scm` file includes a copy of the `switch` macro from [moremacros][chicken moremacros] written by Kon Lovett.

[sisl github]: https://github.com/agra-uni-bremen/sisl
[chicken web]: https://call-cc.org/
[symex-vp sps]: https://github.com/agra-uni-bremen/sps-vp
[symex-vp github]: https://github.com/agra-uni-bremen/symex-vp
[chicken moremacros]: https://wiki.call-cc.org/eggref/5/moremacros
