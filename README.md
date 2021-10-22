# CPSC 312 Project

TODO
- Add line references to the code (I don't know how to do this)
- Go over the template and make sure we're hitting all the major points
- Submit on PrairieLearn


Project Template for CPSC 312 Projects. Use this for both your proposal and final project submission.

(Since you are submitting a link to a specific commit, we will be able to tell the difference.)

The template to edit begins below. You can delete this header section once you've begun.

We will post some additional material to include when you reach the final project stage.

# Insert My Excellent Project Title Here

This will be replaced with a very brief (one paragraph, 2-3 sentences) overview of the project.

Leave the following sentence in so you can easily link back to the requirements and *especially* rubric while editing your project:

This project is in fulfillment of the [CPSC 312 2021W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html).

## Team Members

Our team is:

+ Shaunak Tulshibagwale (student # 1): 
+ Jason Hsu (student # 2): 
+ Peyton Seigo: Team Member Name 3 (student # 3):
+ Markus de Medeiros

We call ourselves: optional awesome team name 2.

## Product Pitch

Our Pro(ject/duct) is a music visualizer that processes a song with various transformations and creates a visualization of shapes/colors using it.

We hope to write this in a way that allows us to write simple and reusable pure functions to make our visualizations easy to write and flexible to extend. Using polymorphic types, we define an interface which makes the data reading, processing, rendering, and saving steps orthogonal problems which interoperate well with existing libries. 


Replace this with a pitch for your project and the problem it solves. This is your vision for what the project
would like like as a complete product, ready for awesome action. (Yes, awesomeness seems to be a theme.)
It may be as short as a couple of paragraphs, or it may be longer. It should **definitely** take less than 4 minutes
to read carefully and thoroughly.

Be sure that this touches clearly on the [project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html#project-requirements).

Good goals to aim for are from the top two rubric items for proposal grading:

> Exciting and language-appropriate product idea tackling a problem that is clearly compelling to a significant audience.

Or:

> Solid and language-appropriate product idea with a problem that is of real interest to the submitting team.

(It's easy to focus on the product and not the problem. Remember to include both!)

## Minimal Viable Project

- MVP is bar frequency animation
- Fourier transform of signal and display the (normalized) volumes of frequency ranges
- This would demonstrate the strength of our interfaces between data processing and visualization, and would provide a set of essential tools (such as a fourier transform operator on audio samples) which future visualzations could reuse in some manner. 
- This generality demonstrates the power of Haskell's polymorphic interfaces and typeclasses. In particular, it is a good application of referential transparency and pure interfaces, because we will be able to write pure visualizations that work on arbitrary data we pull out of an audio sample completely independently of how or what we decide to pull out from an audio file! 


Replace this with a description of the minimal viable project you will actually build for CPSC 312 (if this becomes your final project).
It may be as short as a few paragraphs, or it may be longer. It should **definitely** take less than 4 minutes
to read carefully and thoroughly.

Make clear:
+ how this builds meaningfully toward your product pitch above, without being nearly as much work,
+ how it builds on the strength and power of the language, and
+ how it leads naturally to learning and applying some new element of the language (including what that element is!)

Good goals to aim for are from the top two rubric items for proposal grading:

> The minimal viable project (MVP) builds on the strengths and power of the language in exciting ways that will clearly lead to excellent learning for students.

Or:

> The MVP clearly builds significantly on the language and will lead in interesting and natural ways to learning for the students.

## Proof of Concept

The major obstacle to the viability of this project is the outside effects of reading audio files and rendering data, so we wrote that aspect as our proof of concept.

The test conduit reads data from `data/test_data.flac` and outputs a video `data/output.mp4` created by reading every frame of the audio file and putting out a static frame for every image. Examining the output anumation, you can see the zoomed in Haskell logo resizing itself in response to audio samples. We've signifigantly slown down the test animation so that this is visible. 

Our proof of concept demonstrates that conduits are a viable strategy to eliminating side effects from the design of data filters. Pulling all our effects into a conduit allows us to write idiomatic haskell code at each stage without worrying about the actual process of reading and displaying the files. Seperating business logic from effects is a major benefit of Haskell, and we predict it will make implementing the mathematical procedures of audio and video effects (for example, computing fourier transforms or applying colour maps onto a rendered svg frame) signifigantly easier. 

To explore our pipeline further, you can write more data filters in `processing.hs` which take `SoundStream`s to `DataStream`s of some type, and change the anonymous graphics function in line 8 of `Lib.hs` to another function from the `reanimate` library. Exploring the types should give you a good idea of the flexibility of our framework to create interesting audio visualizations with a simple, reusable interface.


Replace this with a description of your proof-of-concept. This may be as short as a few paragraphs, or it may be longer.
It should **definitely** take less than 4 minutes to read carefully and thoroughly, though working through and running the
code may take an extra 4 minutes. (Your guidance and links should make it easy for us to work through the code.)

Tell us:

+ what key element of your project the proof-of-concept focuses on
+ what makes that such an important element
+ how completing this gives you confidence that, with sufficient work, you could complete the full (minimal viable) project

Include links (likely even line-level links, which are easy to create in Github) throughout to critical pieces of
the code to make it easy for us to understand what you've accomplished and how it fulfills the requirements.

Also include instructions for us to test and run your code. (See our guidelines below.)

A good goal to aim for is the top rubric item from proposal grading:

> Fully functional proof-of-concept is easy to use and review, and it clearly demonstrates a key element necessary for the overall project.

### How to test and run the code: Haskell

Replace this section with instructions to us for how to test and run your code.

- The basic audio pipeline can be run just using `stack run`, since our proof of concept is mainly just the impure IO actions which are difficult to unit test. In the future, when we are writing pure functions to apply into our pipeline, they will be easy to test with `stack test`. 

- We use a couple libraries, they should all be in `stack.yaml`. The only one which might cause some difficulty to run is [conduit-audio-sndfile](https://hackage.haskell.org/package/conduit-audio-sndfile) since it is a wrapper around `libsndfile`. This library has had no difficulty on our Manjaro and Ubintu based systems, however we refer you to the docs if this is a serious difficulty on other operating systems. 



Note: We expect to be able to test your code by running `stack test`. Included among your tests should be some that demonstrate the core functionality of your code. (We will be running `make haskell-eval` from the project root.)

We should be able to further explore your code's functionality by running `stack ghci`, and you should instruct us on some interesting cases to try.

If you include instructions different from these, be **absolutely sure** that they will work well for us in whatever environment we run your code and that they will be as easy to use as the instructions above!
