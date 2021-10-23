
# Lucid Visualizations

Lucid Visualization is an audio visualization tool that aims to create lucid, groovy, and ethereal visualizations of your favorite songs.

This project is in fulfillment of the [CPSC 312 2021W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html).

## Team Members

Our team is:

+ Shaunak Tulshibagwale (98695802): nickname
+ Jason Hsu (44346849): nickname 
+ Peyton Seigo (student # 3): nickname
+ Markus de Medeiros (92214535): nickname

We call ourselves: optional awesome team name 2.

## Product Pitch

Music visualizers often are only in the forms of simple frequency bar graphs and thus are a little boring. Meanwhile, the more interesting ones are often closed-source, paid, and inextensible.

To solve this problem, this project aims to be a real-time music visualizer with extensible and interesting animations. The core structure of the project involves reading audio files/streams or recorded audio from an audio device into a [Conduit Audio](https://hackage.haskell.org/package/conduit-audio-0.2.0.3) stream, which could then be consumed and transformed with FFT and other algorithms to isolate different parts of the music or implement beat detection. The transformed audio would then used to create visualizations with [Reanimate](https://hackage.haskell.org/package/reanimate), as default, or applied to other display mediums such as RGB LED light strips for an extra-awesome light show. With this library, a user with some haskell knowledge could easily create awesome visualizations with all varieties of colors, shapes and entropy.

We think Haskell is perfect for this problem because of its focus on wholemeal programming, laziness, and focus on functional composition, which would be conducive to working
with potentially infinite audio streams and transforming that data in multiple ways.




## Minimal Viable Project

Our Minimal Viable Project would be able to:
- Read any audio file in FLAC format, of a certain size
- Process the produced audio stream by applying a fourier transform to the signal
- Display the normalized volumes as a bar graph frequency animation or a reactive line drawing

Note: Each visualization is unique, but deterministic. If two completely different songs are entered, they should have different, equally valid visualizations. If the same song is inputted, the visualization should be the same

In this project, we will be building on our Haskell learning by incorporating and learning about the programming patterns of at least 2 substantial 3rd-party libraries (Conduit and Reanimate), using Haskell's philosophy of wholemeal programming in working with audio sample streams, and taking advantage of Haskell's unique brand of polymorphism to create flexible and extensible library that allows all kinds of modifying functions to be plugged into the transformation and visualization stages of file processing.


## Proof of Concept


Our current POC proves we are able to:
- Read data from an audio file
- Apply a filter to that data
- Create a framework that allows us to write pure functions which turn arbitrary data into animation frames. 
- Render a test video and write actual frames to a file from an audio source input.

To extend our POC to the MVP and beyond, we simply have to a) write functions that we can plug into the current process that produce more interesting animations and b) ensure that the filters we apply do not take too long to run.

The POC focuses on the key concepts of reading audio files and rendering a sample. Some relevant links

https://github.students.cs.ubc.ca/shaunakt/cpsc312-project-new/blob/main/haskell/src/Sources.hs#L10-L14 - reading an audio stream

https://github.students.cs.ubc.ca/shaunakt/cpsc312-project-new/blob/main/haskell/src/Lib.hs#L24-L27 - Main function

https://github.students.cs.ubc.ca/shaunakt/cpsc312-project-new/blob/main/haskell/src/Dataflow.hs - Fixing type constraints to ensure files match up


### How to test and run the code: Haskell

- The basic audio pipeline can be run just using `stack run`, but we've also put an alias to running main in `stack test`. The test pipeline reads data from `data/test_data.flac` and outputs a video `data/output.mp4` created by reading every frame of the audio file and putting out a static frame for every image. 

- We use a couple libraries, they should all be in `stack.yaml`. The only one which might cause some difficulty to run is [conduit-audio-sndfile](https://hackage.haskell.org/package/conduit-audio-sndfile) since it is a wrapper around `libsndfile`. 


As it is currently set up, editing works best if you first `cd` into the `haskell` subdirectory and open VS Code on that directory (`code .`). There is a `Makefile` with some helpful aliases, but you can also just use `stack` as normal.


By running `stack ghci`, you can use the function `basicAudioPipeline` which has the signature:
`basicAudioPipeline :: FilePath -> (SoundStream a -> DataStream Double) -> (Double -> SVG) -> IO ()` 
It can be called with different filters and functions to see how it works.
