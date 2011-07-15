# Gen-Event Registrar

This application provides a simple mechanism for supervising `gen_event` event
handlers, packaged as an OTP library for convenient reuse.

## Rationale

OTP's `gen_event` mechanism is different from the other OTP behaviours, because
the (user) callback module does not represent (i.e, is not run in) its own,
separate process. Using the standard OTP `gen_event` mechanism, an event hander
callback module that crashes will be silently removed from its event manager.
Because the event handler is not a process, it is not therefore possible to have
the handler participate in a supervision tree. To compensate for this apparent
lack, the `gen_event` module exports an `add_sup_handler/3` method which offers
the calling process a contract which promises notification of exit/error signals
in the callback module, delivered to its message queue.

This library takes advantage of this *supervision* mechanism, wrapping up all the
details and providing a pre-packaged supervision tree for event managers and 
handlers, with a simple API and configuration system for managing all the above.

## License

The `gen_event_registrar` package is licensed under a permissive, BSD-like
license, which allows for combined with other works under different (sub) license
conditions.

## Releases

This project will use [semantic versioning](http://www.semver.org), and all major
releases will be made available as binary packages via github downloads and via
the [nebularis-maven-repository](https://github.com/nebularis/maven-repo).
