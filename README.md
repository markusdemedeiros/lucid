# Lucid Visualizations

Lucid Visualization is an audio visualization tool that aims to create lucid, groovy, and ethereal visualizations of your favorite songs.

This project is in fulfillment of the [CPSC 312 2021W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html).

### Video Demonstration and Supplimentary Links: 
+ [Project Explanation Video](https://youtu.be/AcDt1_8YBQw)
+ [Render 0: MVP](https://youtu.be/lejykprmD3I)
+ [Render 1: Africa](https://youtu.be/_OdjW5A2Vjw)
+ [Render 2: Log Sine Sweep](https://youtu.be/zUjDS8iYG_k)
+ [Render 3: Cantina Band](https://youtu.be/Q1asUrntCQc)
+ [Render 4: Cloud Kisser](https://youtu.be/bix2TiaBb40)


### Dependencies

- This project requires `ffmpeg`: either from your package manager of choice or downloaded off of their [website](https://ffmpeg.org/download.html). 

- This project requires `libsndfile`: either from your package manager of choice or built from [source](https://github.com/libsndfile/libsndfile).

- Usage: `stack build -- exec "lucid-exe [args]"`. The `-h` flag will provide up to date usage information. 

- The basic functionality of the program can be invoked via `stack build --exec "lucid-exe -i [PATH]` where `PATH` is the path to a `.wav` file. This will create two files in the same directory as the `.wav` file: one without audio and one 
