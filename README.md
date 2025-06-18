# ctx-pca-localize

This repository contains all the necessary data and scripts for the analysis of simulation runs described in the paper "Estimating Correlations in Low-Reliability Settings With Constrained Hierarchical Models" by Mehrvarz & Rouder (2025). This paper has been accepted for publication in *Behavior Research Methods*.

## Repository Structure

- `/_data`
  - `/_sim-results`: Contains the summary performance metrics from the simulations.
  - `/_ground-truth`: Contains correlation, covariance, and factor structure for each ground truth used for simulation.
  - `/_coverage-results`: Contains the summary of coverage performance.

- `/_logistics`: Contains the functions and models used for the simulations.

- `/_make-figs`: Contains scripts used to generate plots.

- `/_manuscript`: Contains manuscript-relevant materials.

- `/_scripts`: Contains data wrangling and cleaning scripts.

- `/_sims`: Contains the simulation scripts.

To obtain the raw jags model outputs from simulation runs or coverage runs, you can use `bigRun.R` and `coverageRun.R`, respectively. These scripts will generate raw model outputs in the `_data/_sim-runs` and `_data/_coverage-runs` directories. Please note that the parallel processing cores are preset to 18. To modify this setting, adjust the `num_cores` parameter accordingly.

## Citation

To cite the work related to this repository, please use the following citation:

```bibtex
@article{mehrvarz2025estimating,
  title={Estimating correlations in low-reliability settings with constrained hierarchical models},
  author={Mehrvarz, Mahbod and Rouder, Jeffrey N},
  journal={Behavior Research Methods},
  volume={57},
  number={2},
  pages={59},
  year={2025},
  publisher={Springer}
}```

