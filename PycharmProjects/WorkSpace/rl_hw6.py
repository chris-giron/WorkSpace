import pandas as pd
import numpy as np


def addToClipboard(text_list):
    df = pd.DataFrame([','.join(text_list)])
    df.to_clipboard(index=False,header=False)


answers = ["NO FIGHT", "I DON'T KNOW", "NO FIGHT", "NO FIGHT", "NO FIGHT", "NO FIGHT", "NO FIGHT", "NO FIGHT"]


numOfPatrons = 2
atEstablishment = np.array([[1,1],[0,1],[1,1],[0,1],[0,1],[0,1],[1,1],[1,1]])
fightOccurred = np.array([0,0,0,0,0,0,0,0])

addToClipboard(answers)





