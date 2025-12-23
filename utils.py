from trueskill import Rating, rate, rate_1vs1
import pandas as pd


def calculate_qscore(df):
    """
    Compute Strenght of Schedule (SOS) q scores for pair-wise comparisons in a dataframe.
    The formula already bounds the scores in [0, 10]

    Parameters
    ----------
    df : DataFrame
        The different pair-wise (or 1 vs 1) comparisons on each row under
        given value of the column `Question`

    Returns
    -------
    DataFrame
        A n x 4 Dataframe with n unique images and 3 columns (id, question,
        score, and num. of comparisons)
    """
    # placeholder for final list and dataframe
    q_scores_list = []

    indicators = df["Question"].unique()
    all_players = df["Left_image"].unique()

    for ind in indicators:
        df_ind = df[df["Question"] == ind]

        counts_player1_df = df_ind.groupby(
            "Left_image")["Score"].value_counts()
        counts_player2_df = df_ind.groupby(
            "Right_image")["Score"].value_counts()

        # dictionaries to hold information of every image {image: values}
        W_i_u_dict, L_i_u_dict = {}, {}
        n_i_w_dict, n_i_l_dict = {}, {}
        num_comparisons_dict = {}
        # dictionary of images that image was selected over
        compared_w_dict = {}  # {image: [image1, image2, ...]}
        # dictionary of images that image was NOT selected over
        compared_l_dict = {}  # {image: [image1, image2, ...]}

        # Calculate W_i_u, L_i_u for all images/players
        for player in all_players:
            # number of times an image was selected over its paired image
            # player won as player1 (left)
            player1_w = df_ind[
                (df_ind["Left_image"] == player) & (df_ind["Score"] == "left")
            ]
            # player won as player2 (right)
            player2_w = df_ind[
                (df_ind["Right_image"] == player) & (
                    df_ind["Score"] == "right")
            ]
            # swap columns to have player only as the Left image
            aux = player2_w["Left_image"]
            player2_w.loc[:, "Left_image"] = player2_w.loc[:, "Right_image"]
            player2_w.loc[:, "Right_image"] = aux
            # stack dataframes into one
            player_w = pd.concat([player1_w, player2_w], ignore_index=True)
            w = len(player_w)
            n_i_w_df = player_w.drop_duplicates(
                subset=["Left_image", "Right_image"])
            n_i_w_dict[player] = len(n_i_w_df)

            # number of times an image was NOT selected over its paired image
            # player lost as player1 (left)
            player1_l = df_ind[
                (df_ind["Left_image"] == player) & (df_ind["Score"] == "right")
            ]
            # player lost as player2 (right)
            player2_l = df_ind[
                (df_ind["Right_image"] == player) & (df_ind["Score"] == "left")
            ]
            # swap columns to have player only as the Left image
            aux = player2_l["Left_image"]
            player2_l.loc[:, "Left_image"] = player2_l.loc[:, "Right_image"]
            player2_l.loc[:, "Right_image"] = aux

            # stack dataframes into a single one
            player_l = pd.concat([player1_l, player2_l], ignore_index=True)
            l = len(player_l)
            n_i_l_df = player_l.drop_duplicates(
                subset=["Left_image", "Right_image"])
            n_i_l_dict[player] = len(n_i_l_df)

            # number of times when image was chosen as equal
            t1 = counts_player1_df.get(player, {}).get("equal", 0)
            t2 = counts_player2_df.get(player, {}).get("equal", 0)
            total = w + l + t1 + t2  # same as num_comparisons

            # W and L rate
            if total != 0:
                W_i_u_dict[player] = w / total
                L_i_u_dict[player] = l / total
            else:
                W_i_u_dict[player], L_i_u_dict[player] = 0, 0

            num_comparisons_dict[player] = total
            compared_w_dict[player] = n_i_w_df["Right_image"].unique()
            compared_l_dict[player] = n_i_l_df["Right_image"].unique()
            # end loop for players

        # Calculate W_j_u_sum, L_j_u_sum
        for player in all_players:
            # sum the win ratio of images that player was selected over
            W_j_u_sum = sum(
                w_i_u
                for win_player, w_i_u in W_i_u_dict.items()
                if win_player in compared_w_dict[player]
            )
            # sum the win ratio of images that player was NOT selected over
            L_j_u_sum = sum(
                l_i_u
                for lose_player, l_i_u in L_i_u_dict.items()
                if lose_player in compared_l_dict[player]
            )

            # Calculate Q score
            q_score = (10 / 3) * (
                W_i_u_dict[player]
                + (1 / (n_i_w_dict[player] or 0.001)) *
                W_j_u_sum  # in case the number is 0
                - (1 / (n_i_l_dict[player] or 0.001)) *
                L_j_u_sum  # in case the number is 0
                + 1
            )

            # build the final dataframe
            q_scores_list.append(
                {
                    "Image": player,
                    "Question": ind,
                    "Score": q_score,
                    "Num_comparisons": num_comparisons_dict[player],
                }
            )

        # end loop for indicators
    df_q_scores = pd.DataFrame(q_scores_list)

    return df_q_scores


def calculate_qscore_by_demographic(df, demographic, min_filter=0):
    """
    Compute Strenght of Schedule (SOS) q scores for each value of
    a demographic `Question`

    Parameters
    ----------
    df : DataFrame
        The different pair-wise (or 1 vs 1) comparisons on each row under
        given value of the column `Question`
    demographic : String
        Column name of the demographic question to slice the DataFrame
    min_filter: int
        Minimum num. of comparisons per image to use as threshold.
        The default value of 0 means will use all records

    Returns
    -------
    dictionary
        A dictionary of {question_value: DataFrame} where question_value is the
        unique value of the feature question and DataFrame is the result of
        calculating the trueskill scores for the filtered DataFrame
    """

    # placeholder variable
    q_scores_df = {}

    # for each value of the demographic question, calculate trueskill scores
    for d in df[demographic].unique():
        df_filtered = df[df[demographic] == d]

        if min_filter == 0:  # don't filter at all
            q_scores_df[d] = calculate_qscore(df_filtered)
        else:  # keep images with at least `min_filter` num. of comparisons
            df_scores = calculate_qscore(df_filtered)
            q_scores_df[d] = df_scores[df_scores['Num_comparisons']
                                       >= min_filter]

    return q_scores_df


def calculate_trueskill(df, scaling=False, normal_dist=False):
    """
    Compute trueskill scores for pair-wise comparisons in a dataframe

    Parameters
    ----------
    df : DataFrame
        The different pair-wise (or 1 vs 1) comparisons on each row under
        given value of the column `Question`
    scaling : Boolean
        Flag to scale the final values into the range [0,10] to match
        pre-trained models on PP2
    normal-dist Boolean
        Flag to compare games against a dummy normal distribution

    Returns
    -------
    DataFrame
        A n x 4 Dataframe with n unique images and 3 columns (id, question,
        score, and num. of comparisons)
    """
    # dictionary for all indicators, each image is a player
    all_ratings = {}  # {indicator: {player: ratings} }
    all_counts = {}  # {indicator: {player: counts} }

    indicators = df['Question'].unique()

    # default ratings for each new player
    default_rating = Rating()
    default_count = 0

    # placeholder for final list and dataframe
    trueskills_scores_list = []

    for ind in indicators:
        df_ind = df[df['Question'] == ind]

        # initialize a dictionary to hold each player's TrueSkill rating and
        # number of comparisons
        ratings = {}  # {player: ratings}
        counts = {}  # {player: counts}

        for _, row in df_ind.iterrows():
            player1 = row['Left_image']
            player2 = row['Right_image']
            result = row['Score']

            # get the value based on player key, if not key doesn't exist give
            # default values
            rating1 = ratings.get(player1, default_rating)
            rating2 = ratings.get(player2, default_rating)
            count1 = counts.get(player1, default_count)
            count2 = counts.get(player2, default_count)

            # Update ratings based on the result of the match
            if result == 'left':
                # Player1 wins
                if normal_dist:
                    rating1, _ = rate_1vs1(rating1, Rating())
                else:
                    rating1, rating2 = rate_1vs1(rating1, rating2)
            elif result == 'right':
                # Player2 wins
                if normal_dist:
                    rating2, _ = rate_1vs1(rating2, Rating())
                else:
                    rating2, rating1 = rate_1vs1(rating2, rating1)
            elif result == 'equal':
                # Draw
                if normal_dist:
                    (rating1, _), (_, rating2) = rate(
                        [(rating1, Rating()), (Rating(), rating2)], ranks=[0, 0])
                else:
                    (rating1,), (rating2,) = rate(
                        [(rating1,), (rating2,)], ranks=[0, 0])

            # update variables for next games
            counts[player1] = count1 + 1
            counts[player2] = count2 + 1
            ratings[player1] = rating1
            ratings[player2] = rating2

            # end for loop for rows

        all_ratings[ind] = ratings
        all_counts[ind] = counts

        # start building the final list
        for player, rating in ratings.items():
            trueskills_scores_list.append({
                'Image': player,
                'Question': ind,
                'TrueSkill_score': rating.mu,
                'Num_comparisons': counts[player]
            })
        # end for loop for indicators

    # transform to dataframe
    df_trueskills = pd.DataFrame(trueskills_scores_list)

    if scaling:  # scales values [0, 10]
        # theoretical trueskill is [10, 40]
        min_trueskill = 10
        max_trueskill = 40
        df_trueskills['TrueSkill_score'] = 10 * (df_trueskills['TrueSkill_score'] - min_trueskill) / (
            max_trueskill - min_trueskill)

    return df_trueskills


def calculate_trueskill_by_demographic(df, demographic, min_filter=0, scaling=False, normal_dist=False):
    """
    Compute trueskill scores for each value of a demographic `Question`

    Parameters
    ----------
    df : DataFrame
        The different pair-wise (or 1 vs 1) comparisons on each row under
        given value of the column `Question`
    demographic : String
        Column name of the demographic question to slice the DataFrame
    min_filter: int
        Minimum num. of comparisons per image to use as threshold.
        The default value of 0 means will use all records
    scaling : Boolean
        Flag to scale the final values into the range [0,10] to match
        pre-trained models on PP2
    normal-dist Boolean
        Flag to compare games against a dummy normal distribution

    Returns
    -------
    dictionary
        A dictionary of {question_value: DataFrame} where question_value is the
        unique value of the feature question and DataFrame is the result of
        calculating the trueskill scores for the filtered DataFrame
    """
    # placeholder variable
    trueskill_scores_df = {}

    # for each value of the demographic question, calculate trueskill scores
    for d in df[demographic].unique():
        df_filtered = df[df[demographic] == d]

        if min_filter == 0:  # don't filter at all
            trueskill_scores_df[d] = calculate_trueskill(
                df_filtered, scaling=scaling, normal_dist=normal_dist)
        else:  # keep images with at least `min_filter` num. of comparisons
            df_scores = calculate_trueskill(
                df_filtered, scaling=scaling, normal_dist=normal_dist)
            trueskill_scores_df[d] = df_scores[df_scores['Num_comparisons']
                                               >= min_filter]

    return trueskill_scores_df
