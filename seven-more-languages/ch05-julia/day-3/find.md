## Julia's documentation on modules and packages

* [Modules](http://docs.julialang.org/en/release-0.4/manual/modules/)
* [Packages](http://docs.julialang.org/en/release-0.4/manual/packages/)

## How JPEG works

Found some description in wikipedia: [JPEG](https://en.wikipedia.org/wiki/JPEG#Encoding)

Encoding steps are:

* Color space transformation: Transforms RGB color space into some other space
* Downsampling: Reduce the spatial resolution of some components of the color space
* Block splitting: split image into small blocks
* Discrete cosine transform: convert each block of each component into a frequency-domain representation
* Quantization: apply masks to blocks, the amount of information in high frequency components are reduced
* Entropy coding: a lossless data compression process

For the example in book, we only deal with grayscale images and
there is no need for color space transformation and downsampling (looks like this could
only work with YCbCr model. Also the final step entropy coding is left out.
