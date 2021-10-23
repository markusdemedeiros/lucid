
# Lucid Visualizations

Lucid Visualization is an audio visualization tool that aims to create lucid, groovy, and ethereal visualizations of your favorite songs.

This project is in fulfillment of the [CPSC 312 2021W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html).

## Team Members

Our team is:

+ Shaunak Tulshibagwale (student # 1): 
+ Jason Hsu (student # 2): 
+ Peyton Seigo: (student # 3):
+ Markus de Medeiros: (student # 4)

We call ourselves: optional awesome team name 2.

## Product Pitch

This project aims to create visualizations of audio files (songs) by reading and applying transformations to audio files (using [Conduit Audio](https://hackage.haskell.org/package/conduit-audio-0.2.0.3)) which could include fourier transforms, frequency isolations, etc. The transformed audio is then used with [Reanimate](https://hackage.haskell.org/package/reanimate), a haskell animation library based on SVG's. 
We hope that in completion, this project will create unique visualizations that make use of many libraries available to transform audio. We also intend to use this audio to incorporate varieties of colors, shapes and entropy into the visualizations. Currently, we have Conduit Audio for our audio transformations, but later we could use more sophisticated libraries to get cooler audio transformations/visualizations.
We also hope for visualizations to be usable by external devices such as LED lights, LED light strips for a more lively musical experience. The visualizations, transformations, will all have run in real time for a timely visual.
The proof-of-concept has been written to make our code flexible; By using interfaces that use polymorphic types, the steps of reading, processing, rendering, and saving data will also be polymorphic.


(It's easy to focus on the product and not the problem. Remember to include both!)

## Minimal Viable Project

Our Minimal Viable Project would be to a create a visualization that:
- Reads any audio file in a .FLAC format, of a certain size
- Processes audio by applying a fourier transform of the signal
- Displays the normalized volumes as a bar frequency animation
- Visualization is unique, but deterministic. If two completely different songs are entered, they should have different, equally valid visualizations
- If the same song is inputted, the visualization should be the same

This project is a simplified version of the initial pitch as it simply creates a bar frequency animation, instead of a visualization that has different colors and shape and is a little more interesting to look at. It builds meaningfully towards the pitch however, by being able read and write, and visualize audio. It also lends itself to be flexible into what kinds of functions we can plug into the transformation and visualization stages of the file processing, by using polymorphic types.


## Proof of Concept


Our current POC does the following:
- Reads data from an audio file
- applies filters to that data
- Allows us to write pure functions which turn arbitrary data into animation frames. 
- Renders a test video proving that it is possible to write actual frames from an audio source.

To extend our POC to the MVP and beyond, we simply have to a) write functions that we can plug into the current process that produce more interesting animations and b) ensure that the filters we apply do not take too long.

The POC focuses on the key concepts of reading audio files and rendering a sample. Some relevant links

https://github.students.cs.ubc.ca/shaunakt/cpsc312-project-new/blob/main/haskell/src/Sources.hs#L10-L14 - reading an audio stream

https://github.students.cs.ubc.ca/shaunakt/cpsc312-project-new/blob/main/haskell/src/Lib.hs#L24-L27 - Main function

https://github.students.cs.ubc.ca/shaunakt/cpsc312-project-new/blob/main/haskell/src/Dataflow.hs - Fixing type constraints to ensure files match up


### How to test and run the code: Haskell

Replace this section with instructions to us for how to test and run your code.

- The basic audio pipeline can be run just using `stack run`, but we've also put an alias to running main in `stack test`. The test pipeline reads data from `data/test_data.flac` and outputs a video `data/output.mp4` created by reading every frame of the audio file and putting out a static frame for every image. 

- We use a couple libraries, they should all be in `stack.yaml`. The only one which might cause some difficulty to run is [conduit-audio-sndfile](https://hackage.haskell.org/package/conduit-audio-sndfile) since it is a wrapper around `libsndfile`. 


As it is currently set up, editing works best if you first `cd` into the `haskell` subdirectory and open VS Code on that directory (`code .`). There is a `Makefile` with some helpful aliases, but you can also just use `stack` as normal.

We should be able to further explore your code's functionality by running `stack ghci`, and you should instruct us on some interesting cases to try.

By running `stack ghci`, you can use the function `basicAudioPipeline` which has the signature:
`basicAudioPipeline :: FilePath -> (SoundStream a -> DataStream Double) -> (Double -> SVG) -> IO ()` 
It can be called with different filters and functions to see how it works.

### How to test and run the code: Prolog

Replace this section with instructions to us for how to test and run your code.

Instructions coming soon, but we expect you'll use the [Prolog Unit Testing](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)) library for testing and that we'll be able to run your code with `swipl`.


