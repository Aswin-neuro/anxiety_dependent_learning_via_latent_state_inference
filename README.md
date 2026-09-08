# Anxiety-Dependent Learning via Latent State Inference

## Goal: before the actual implementation, we wanted to test how pavlovian bias can affect state learning vs continuous learning.

This repository implements a computational cognitive model to understand how **anxiety and conflict modulate learning** in humans and simulated agents. We use **Hidden Markov Models (HMMs)** to infer latent task blocks (high/low conflict, medium conflict) from observed behavior (choices and rewards), extending classical reinforcement learning theory.

## Quick Start

1. **Install dependencies:**
   ```bash
   pip install -e .
   ```

2. **Run the full pipeline:**
   - Execute notebooks in order: `01_mle` → `02_simulation` → `03_hmm_inference` → `04_analysis`
   - Each notebook saves outputs to `data/` for downstream use

3. **Review results:**
   - HMM state assignments + transition matrices in `03_hmm_inference/4_hmm_action_outcome_valence.ipynb`
   - Agent Q-value and surprise reconstructions in `04_analysis/5_tracking_agent_qvals_sv.ipynb`

---

## Project Components

### 1. **MLE Fundamentals** (`notebooks/01_mle_fundamentals/`)

**Notebook:** `1_maximum_likelihood_estimation_coin_toss.ipynb`

**Purpose:**  
Introductory tutorial on Expectation-Maximization (EM) estimation using a toy coin-toss example. Builds intuition for latent variable inference applied later to task block detection.

**Key Concepts:**
- Hidden variable: which of two coins is being tossed
- Observed data: heads/tails sequence
- Iterative E-step (estimate hidden coin identity) and M-step (update coin bias parameters)

**Inputs:** None (synthetic data)

**Outputs:** None (educational only)

---

### 2. **Simulation & Parameter Recovery** (`notebooks/02_simulation/`)

**Notebook:** `2_gng_simulation_and_parameter_recovery.ipynb`

**Purpose:**  
Simulate an RL agent performing a Go/No-Go task under varying **conflict blocks**:
- **MC (Medium Conflict):** Balanced approach/avoid trials  
- **HC (High Conflict):** Mostly approach-bias (positive cues) inducing response conflict  
- **LC (Low Conflict):** Mostly avoid-bias (negative cues) with minimal conflict

Generate synthetic data and validate parameter recovery using maximum likelihood.

**Key Concepts:**
- Block structure: PC (Pavlovian Congruent - Low conflict), PI (Pavlovian Incongruent - High conflict)
- Cue valences: -1 (Loss), +1 (Win)
- Rescorla-Wagner learning updates on Go/No-Go values
- Parameter recovery: can we re-fit agent parameters from synthetic trajectories?

**Inputs:** 
- `data/raw/blank_environment.csv` – empty task setup
- `data/raw/pure_task_environment.csv` – conflict block definitions
- Other reference CSVs in root

**Outputs:**
- `data/interim/simulated_agent_4000T.csv` – main synthetic dataset (4000 trials)
- `data/interim/simulated_agent_400T.csv` – small version for testing
- Parameter recovery plots and accuracy summaries

**How to Use:**
1. Open the notebook and run all cells in order
2. Inspect "Per-Block Summary" and "Per-Trial-Type Accuracy" sections
3. Check parameter recovery scatter plots (recovered β_go vs. true β_go)
4. Copy the canonical dataset path for downstream notebooks

---

### 3. **HMM Block Inference** (`notebooks/03_hmm_inference/`)

#### **3a. Action-Outcome Only** 
**Notebook:** `3_hmm_action_outcome.ipynb`

**Purpose:**  
Fit a 3-state categorical HMM to agent **choices and rewards** alone (ignoring cue valence) to infer which hidden block the agent is in over time.

**Key Concepts:**
- Observation encoding: (Agent_Choice, Agent_Reward) → integer 0–5  
- 3 hidden states ≈ {low-conflict, medium-conflict, high-conflict} or similar latent grouping  
- Viterbi decoding to assign state sequence
- Emission and transition matrices reveal block-specific behavior signatures

**Inputs:**
- `data/interim/simulated_agent_4000T.csv`

**Outputs:**
- Transition matrix and emission probabilities
- Confusion matrix: True Block vs. Inferred HMM State
- `data/processed/hmm_predictions_action_outcome.csv` (if saved)

**How to Use:**
1. Load the simulated data from Step 2
2. Create observation tuples and fit the HMM
3. Examine the transition matrix—high self-transition indicates stable blocks
4. Check emission probabilities: which (choice, reward) pairs favor each state?

---

#### **3b. Action-Outcome-Valence (Recommended)**
**Notebook:** `4_hmm_action_outcome_valence.ipynb`

**Purpose:**  
Extend HMM inference by including **cue valence** in the observation. This richer feature set often improves block discrimination because MC and LC blocks differ in cue composition.

**Key Concepts:**
- Observation encoding: (Cue_Valence, Agent_Choice, Agent_Reward) → integer 0–11  
- Same 3-state HMM but with 12 emission states  
- Better separation between LC and MC blocks vs. action-outcome alone


### 4. **Post-Hoc Analysis: Q-Values & Surprise** (`notebooks/04_analysis/`)

**Notebook:** `5_tracking_agent_qvals_sv.ipynb`

**Purpose:**  
Given the **inferred HMM block assignments**, reconstruct the agent's trial-by-trial **Q-values** (learned values for Go/No-Go under each cue) and **stimulus value (SV)** to understand what the agent "knew" at each trial.

**Key Concepts:**
- Forward integration: simulate agent learning replay using inferred blocks
- Per-trial Q-value estimates for cue combinations
- Prediction error / surprise quantification
- Temporal correlation of surprise with state transitions

**Inputs:**
- `data/interim/simulated_agent_4000T.csv`
- `data/processed/hmm_predictions_full.csv` (state assignments from 3b)

**Outputs:**
- `data/processed/agent_qvals_sv_by_trial.csv`
- Visualizations: Q-value trajectories, surprise peaks at block boundaries

**How to Use:**
1. Load simulated data and HMM state predictions
2. Initialize Q and SV values; loop through trials
3. For each trial, retrieve the inferred block label and apply the appropriate RL update
4. Inspect surprise curves around known block transitions
5. Export for further analysis (e.g., correlation with fMRI/behavior)

---

## Bayesian Hierarchical Model

**File:** `models/gng_m3.stan`

A **Stan** specification for fitting the full hierarchical model across subjects (if real data is available). Defines:
- **Latent parameters** per subject: `xi` (inverse temperature), `ep` (learning rate), etc.
- **Trial-level regressors** generated from Q-values and surprise: `Qgo`, `Qnogo`, `Wgo`, `Wnogo`, `SV`
- Generative model for binomial choice likelihood

**How to Use:**
- Fit with `pymc3` or `cmdstanpy` on real behavioral or fMRI data
- Posterior draws of latent params reveal individual differences in learning under conflict

---

## Data Schema

### Key Columns in Simulated Datasets

| Column | Values | Meaning |
|--------|--------|---------|
| `Trial` | 1–4000 | Trial index |
| `Block` | `B1_MC`, `B2_HC1`, `B3_HC2`, `B4_LC` | Task block label |
| `Conflict` | `MC`, `HC`, `LC` | Conflict level (simplified) |
| `Trial_Type` | `PC`, `PI`, `NW`, `GAL` | Conflict sub-type (Punished-response vs. inaction, Go/No-Go aligned/opposed) |
| `Cue_Valence` | –1, +1 | Cue affective value (negative/positive) |
| `Optimal_Action` | 0, 1 | Correct choice (No-Go=0, Go=1) |
| `Agent_Choice` | 0, 1 | Agent's choice |
| `Agent_Reward` | –1, 0, +1 | Outcome |
| `Correct` | 0, 1 | Whether agent's choice matched optimal |
| `hmm_state` | 0, 1, 2 | Inferred latent block (added after HMM fitting) |

---

## Workflow & Execution Order

```

## Installation

Clone the repository and install dependencies:

```bash
git clone https://github.com/Aswin-neuro/anxiety_dependent_learning_via_latent_state_inference.git
cd anxiety_dependent_learning_via_latent_state_inference
pip install -e .
```

**Dependencies:**
- `jupyter`, `jupyterlab` – notebook environment
- `numpy`, `pandas`, `scipy` – data handling & optimization
- `scikit-learn` – parameter recovery, clustering
- `hmmlearn` – HMM fitting
- `pymc3`, `arviz` – Bayesian inference (optional)
- `cmdstanpy` – Stan execution (optional for hierarchical model)
- `matplotlib`, `seaborn` – visualization

---

## Repository Structure

```
anxiety_dependent_learning_via_latent_state_inference/
├── README.md                          ← You are here
├── pyproject.toml                     ← Dependencies
├── .gitignore
├── main.py                            ← Placeholder
├── notebooks/
│   ├── 01_mle_fundamentals/
│   │   └── 1_maximum_likelihood_estimation_coin_toss.ipynb
│   ├── 02_simulation/
│   │   └── 2_gng_simulation_and_parameter_recovery.ipynb
│   ├── 03_hmm_inference/
│   │   ├── 3_hmm_action_outcome.ipynb
│   │   └── 4_hmm_action_outcome_valence.ipynb
│   ├── 04_analysis/
│   │   └── 5_tracking_agent_qvals_sv.ipynb
│   └── exploratory/
│       └── RL_workbook.ipynb
├── models/
│   └── gng_m3.stan
├── data/
│   ├── raw/
│   │   ├── blank_environment.csv
│   │   ├── pure_task_environment.csv
│   │   ├── simulated_agent_400T.csv
│   │   └── simulated_agent_4000T.csv
│   ├── interim/                      ← Generated by Step 2
│   └── processed/                    ← Generated by Steps 3–4
```

---

## Key References & Notes

- **HMM Theory:** `hmmlearn` documentation and Rabiner's classic HMM tutorial
- **RL Framework:** Rescorla-Wagner learning; see notebook cells for parameter definitions
- **Block Design:** Inspired by empirical conflict adaptation paradigms (e.g., Simon task, Stroop variants)

---

## Contributing & Next Steps

To extend this work:
1. **Integrate real behavioral data:** Replace simulated data in Step 2 with human/animal recordings
2. **Fit hierarchical model:** Use `gng_m3.stan` + posterior predictive checks
3. **Cross-validate HMM:** Compare state inference on held-out trials or alternative models (e.g., 4-state, DP-HMM)
4. **Temporal dynamics:** Add drift-diffusion model coupling HMM states to reaction times

---

**Author(s):** Aswin  

