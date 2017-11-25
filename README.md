# free-trek-hs

A study of Free monads and their applications in testing service orchestration, with a Star Trek theme.

*This is a Haskell port of the Scala-based study [free-trek](https://github.com/seance/free-trek).*

**Prerequisites**

- [Stack](https://docs.haskellstack.org/en/stable/README/) 1.3.2+ recommended

**Running**

Run `stack test` in the root directory.

## Free Trek and Haskell

The implementation closely follows ideas from [David Laing's](http://dlaing.org/cofun/posts/free_and_cofree.html) and [Arnaud Bailly's](https://abailly.github.io/posts/free.html) respective posts on Free DSLs and Cofree interpreters, with some modifications, and using types from the Haskell standard libraries where possible.

This version of Free Trek features a similar algebra structure as the Scala based study, but has a (extensible) `Cofree` based interpretation mechanism.

Notable differences to the Scala version include:
- The `Inject` module contains the Sum/Coproduct composition of the system algebras. This code mostly already existed in `Cats`.

- The trace interpreter uses the Sum representation of the types as the trace type. In Scala, a *trait* super type was used.

- There is no need for a `Execution` module, since the `MonadPlan m` type is a constraint (instead of a separate type), and Haskell is clever enough to select a suitable concrete type automatically. The `mtl` package provides the `RWS` type.

For completeness, the setting of the `free-trek` study is presented below in adapted form.

## The Problem

**James T. Kirk** has a problem. The *USS Enterprise* has recently been fitted with a new *multitronic computer*, and while her subsystems are tried and true, the Captain is concerned whether the new computer will correctly *coordinate* those systems.

After all, each expedition into space can feature a number of potentially dangerous encounters! Not only that, but the computer needs to keep track of the remaining *dilithium* and coordinate warp speed accordingly.

We have been given a spec of the Enterprise's systems by **Montgomery Scott**, found in the `Domain` module. Now, we only need to write a highly testable system that will satisfy the intrepid Captain!

## USS Enterprise Operating Manual

*Note!* Before attempting to operate your *Constitution-class* starship, you must first turn the crank shaft to start the ignition!

### Warp Core

The heart of the ship, producing the energy for the other systems.
- Before making warp jumps, you need to convert dilithium crystals equal to the warp speed, or lower the warp factor (to zero if necessary)
- When operating in asteroid fields, you should engage the antimatter reaction assembly

### Warp Drive

The warp-capable drives that propel the starship in the vast void of space.
- To perform a warp jump, tune the warp field coil to desired warp speed
- When in empty space, you should take the opportunity to attenuate the plasma conduit
- Trudging through nebulae, it is important to disentangle the electro-plasma system

### Phasers

Powerful directed-energy beam weapons that operate on various frequencies.
- When encountering hostile space crabs, focus for high frequency optronics, without forgetting to purge the prefire chamber
- When operating near space-time anomalies (a common occurrence), it is wise to rewire the power cell matrix

### Deflector Shields

Highly polarized energetic distortion field emitters that protect the starship.
- When encountering hostile space crabs, go for parallel frequency modulation
- While negotiating asteroid fields, it is prudent to use anti-parallel frequency modulation instead
- Always decompress the graviton emitter when operating near space-time anomalies!
