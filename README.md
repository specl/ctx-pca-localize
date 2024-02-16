# ctx-pca-localize

This repository contains all the necessary data and scripts for the analysis of simulation runs described in the paper "Estimating Correlations in Low-Reliability Settings With Constrained Hierarchical Models" by Mehrvarz & Rouder (2024).

## Repository Structure

- `/_data`
  - `/_sim-results`: Includes the summary performance metrics from the simulations.
  - `/_ground-truth`: Stores correlation, covariance, and factor structure for each ground truth used for simulation.
  - `/_coverage-results`: Contains the summary of coverage performance.

- `/_logistics`: Features the functions and models used for the simulations.

- `/_make-figs`: Contains scripts used to generate plots.

- `/_manuscript`: Stores manuscript-relevant materials.

- `/_scripts`: Includes data wrangling and cleaning scripts.

- `/_sims`: Contains the simulation scripts.

To obtain the raw jags model outputs from simulation runs or coverage runs, you can use bigRun.R and coverageRun.R, respectively. These scripts will generate raw model outputs in the _data/_sim-runs and _data/_coverage-runs directories. Please note that the parallel processing cores are preset to 18. To modify this setting, adjust the num_cores parameter accordingly.

## Citation

To cite the work related to this repository, please use the following citation:

```bibtex
@article{MehrvarzRouder2024,
  title={Estimating Correlations in Low-Reliability Settings With Constrained Hierarchical Models},
  author={Mehrvarz, M. and Rouder, J. N.},
  year={2024},
  month={Feb},
  day={7},
  doi={10.31234/osf.io/sfce5},
  url={https://osf.io/preprints/psyarxiv/sfce5}
}
```

## Preprint

The preprint of the paper can be found [here](https://osf.io/preprints/psyarxiv/sfce5).
