import numpy as np


# CAM Model 

def CAM_experiment(weights,cues):
    
    j = weights[0] + np.dot(cues,weights[1:])

    return j




# GCM Model 

def distance(vec_1, vec_2, weights, p = 2):
    # Checks
    if len(vec_1) != len(vec_2):
        raise ValueError("Vectors must have the same number of dimensions (columns).")
    
    # if not np.isclose(np.sum(weights), 1.0):
        # raise ValueError("Weights must sum to 1.")

    # Compute distance
    d          = np.power(np.abs(vec_1 - vec_2),p)

    # Weight distance
    if weights is not None:
        weighted_d = d * weights
    else:
        weighted_d = d

    # Compute minkowski distance
    return np.power(np.sum(weighted_d), 1/p)




def distance_matrix(stim_cues, ex_cues, weights, p=2):
    """Computes the Minkowski distance between each stimulus cue and all exemplar cues."""

    if stim_cues.shape[1] != ex_cues.shape[1]:
        raise ValueError("Stimulus and exemplar cues must have the same number of features (columns).")

    # if not np.isclose(np.sum(weights), 1.0):
        #raise ValueError("Weights must sum to 1.")

    # Expand dimensions to allow for broadcasting

    diffs            = np.expand_dims(stim_cues, axis=1) - np.expand_dims(ex_cues, axis=0)

    weighted_diffs_p = np.power(np.abs(diffs), p) * weights

    distances        = np.power(np.sum(weighted_diffs_p, axis=2), 1/p)

    return distances


def similarity(distance,c):
    
    sim = np.exp(-c*distance)

    return sim


def GCM_experiment(stim_cues, ex_cues, ex_crit, weights, c):
    """Generates judgments for multiple trials from the GCM model using vectorized operations."""

    # Compute pairwise distances between all stimulus cues and all exemplar cues
    distances = distance_matrix(stim_cues, ex_cues, weights)

    # Compute similarities
    similarities = similarity(distances, c)

    # Compute weighted criterion values and normalize
    weighted_crit = similarities * ex_crit
    judgments = np.sum(weighted_crit, axis=1) / np.sum(similarities, axis=1)

    return judgments


# Mapping Model

def preprocess_cues(cues, crit):

    _, n_cues = cues.shape

    for i in range(n_cues):

        # Calculate correlation
        r = np.corrcoef(crit, cues[:,i])[0, 1]  
        
        # If correlation is negative, reverse cue coding
        if r < 0:
            cues[:,i] = cues[:,i] * -1


    return cues  


def MAPP_experiment(n_cat,cues,ex_cues,ex_crit):

    # Compute exemplar cue/dimension score
    ex_scores =   np.mean(ex_cues, axis=1)

    # Find min and max
    min_val = np.min(ex_scores)
    max_val = np.max(ex_scores)

    # Create n_cat equally spaced category boundaries
    boundaries = np.linspace(min_val, max_val, n_cat + 1) # n_cat+1 points define n_cat bins

    # Assign each exemplar to a category
    categories = np.sum(ex_scores[:, np.newaxis] >= boundaries[:-1], axis=1)

    # Combine exemplar criterion values with their category
    ex_cats = np.array([ex_crit, categories]).T

    # Get unique group labels
    groups = np.unique(ex_cats[:, 1])  

    # Create dictionary of categories and median criterion values
    median_dict = {int(group): np.median(ex_cats[ex_cats[:, 1] == group][:, 0]) for group in groups}

    # Compute the cue/dimension score for each stimulus
    stim_scores = np.mean(cues, axis=1)

    # Categorize stimuli
    stim_cats = np.sum(stim_scores[:, np.newaxis] >= boundaries[:-1], axis=1)

    # If score smaller than minmum of exemplar scores, put it into first category
    stim_cats[stim_cats == 0] = 1

    # If categories are empty fill them with the mean of the two adjecent categories
    all_categories = np.unique(stim_cats)

    # Determine missing categories
    missing_cats = [cat for cat in all_categories if cat not in median_dict]

    # Fill missing using nearest available neighbors
    available_keys = sorted(median_dict.keys())

    for cat in missing_cats:
        # Find the closest lower and upper available keys
        lower_vals = [k for k in available_keys if k < cat]
        upper_vals = [k for k in available_keys if k > cat]

        lower = lower_vals[-1] if lower_vals else None
        upper = upper_vals[0] if upper_vals else None

        if lower is not None and upper is not None:
            median_dict[cat] = (median_dict[lower] + median_dict[upper]) / 2
        elif lower is not None:
            median_dict[cat] = median_dict[lower]
        elif upper is not None:
            median_dict[cat] = median_dict[upper]
        else:
            median_dict[cat] = np.nan

    # Retrieve medians from dictionary
    pred = np.array([median_dict[x] for x in stim_cats])
    
    return pred




# Quick Est

def preprocess_cues_QuickEst(cues, crit):

    n_stim, n_cues = cues.shape

    for i in range(n_cues):

        # Calculate correlation
        r = np.corrcoef(crit, cues[:,i])[0, 1]  
        
        # If correlation is negative, reverse cue coding
        if r < 0:
            cues[:,i] = cues[:,i] * -1

        cue_mean = np.mean(cues[:,i])

        for s in range(n_stim):

            if cues[s,i] > cue_mean:
                cues[s,i] = 1
            else:
                cues[s,i] = 0


    return cues  


def QuickEst_experiment(cues,ex_cues,mem_ex_crit):
   
    n_trials, _ = cues.shape
 
    # Step 1: Compute average criterion value where cue == 0
    avg_crit_0 = []

    for cue_idx in range(ex_cues.shape[1]):
        mask = ex_cues[:, cue_idx] == 0
        avg  = np.mean(mem_ex_crit[mask])
        avg_crit_0.append(avg)

    avg_crit_0 = np.floor(np.array(avg_crit_0)/ 10) * 10 

    # Define: Catch all category (i.e., if all cues = 1)
    max_ex_crit = np.max(mem_ex_crit)

    # Step 2: Sort indices based on avg_crit_0 (ascending)
    sorted_indices = np.argsort(avg_crit_0)

    # Reorder the columns of QEst_cues
    cues_sorted = cues[:, sorted_indices]

    # Also optionally reorder the average criterion values array itself
    avg_crit_0_sorted = avg_crit_0[sorted_indices]

    # Initialize the output matrix 
    judgments = np.empty(n_trials)

    for i in range(n_trials):

        # Step 3: Check when first cue = 0 
        zero_indices   = np.where(cues_sorted[i, :] == 0)[0]
        first_zero_idx = zero_indices[0] if zero_indices.size > 0 else None 

        # Step 4: Predict based on average score 
        judgments[i] = avg_crit_0_sorted[first_zero_idx] if first_zero_idx is not None else max_ex_crit


    return judgments


# Testing
# mem_ex_crit = np.random.normal(ex_crit,1) 
# QuickEst_experiment(QEst_cues,ex_QEst_cues,mem_ex_crit)