# launchpad-csound

""""Instrument"""" thing with Novation's Launchpad Mini and Lisp.

## Usage (WIP)

``` shell
> csound do.csd
```

``` common-lisp
(ql:quickload :launchpad-csound)
(in-package :launchpad-csound)
(setf *csound* (make-instance 'patterns))
(cloud:connect *csound*)
```

## Video example using current (26/07/20) patterns mode

[![patterns](https://img.youtube.com/vi/BVciaw6djtU/0.jpg)](https://www.youtube.com/watch?v=BVciaw6djtU)

## License

MIT

