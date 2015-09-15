# tino

A gnarly system manager / dotfile implementation. You probably won't have much
use for it, personally, but would probably benefit from building your own
personal ecosystem in the same style. I stole this idea from [tpope].

[tpope]: https://github.com/tpope/tpope



## Installing

```bash
wget https://raw.githubusercontent.com/isovector/tino/master/bin/install
chmod +x ./install
./install <features>
```



## Percentile-based Feedback

`tino` now has built-in support for percentile-based feedback. This is a bit you
might want to steal. There are [Haskell bindings][pcfb] for creating data files
and for connecting it with xmonad. These data files [can be converted][pcfbshow]
to the format used by [Noah Slater's percentile-feedback][nslater] graphing
application.

[pcfb]: https://github.com/isovector/tino/tree/master/.xmonad/src
[pcfbshow]: https://github.com/isovector/tino/blob/master/bin/pcfb-show
[nslater]: https://github.com/nslater/percentile-feedback

