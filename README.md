# launchpad-csound

""""Instrument"""" thing with Novation's Launchpad Mini and Lisp.

## Usage

``` shell
> csound do.csd
```

``` common-lisp
(ql:quickload :launchpad-csound)
(in-package :launchpad-csound)
(setf *csound* (make-instance 'patterns))
(cloud:connect *csound*)
```

## Video example using current (04/06/20) patterns mode

[![patterns](https://img.youtube.com/vi/ogdnxAm13zo/0.jpg)](https://www.youtube.com/watch?v=ogdnxAm13zo)

## License

MIT

