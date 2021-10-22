# Project Architecture Notes

- Our project is divided into four mostly orthogonal stages.
- Stages have defined interfaces in the type system
- Stages are joined by conduits
- Pushing IO into the conduit makes it so that each stage can be designed as pure functions



## 1. Data Sources

- We use the `conduit-audio-sndfile` library ([docs](https://hackage.haskell.org/package/conduit-audio-sndfile-0.1.2.1/docs/Data-Conduit-Audio-Sndfile.html)) which provides a Haskell wrapper around the C ``hsndfile`` library, to provide a performant audio stream from a file
- This stage produces a ``SoundStream a`` conduit


## 2. Data Processing 

- Data is taken into this stage as an ``Audiosource m a`` from the ``conduit-audio`` library (``Data.Conduit.Audio``)
- The ``conduit-audio`` [docs](https://hackage.haskell.org/package/conduit-audio-0.2.0.3/docs/Data-Conduit-Audio.html) provide primitive functions that can be applied to an audio source. 
- The Fast Fourier transform library (?) 
- Data from this stage is output as a (??) type


## 3. Graphics
- 

## 4. Frontend
- This stage is not implemented yet, and may not be implemented.  

