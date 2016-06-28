import json
import pandas as pd

files_in = ['g:/misc/twitter_brexit/twitter_text_0621_0126_0153pm.txt',
'g:/misc/twitter_brexit/twitter_text_0621_0306_0420pm.txt',
'g:/misc/twitter_brexit/twitter_text_0621_0516_0547pm.txt',
'g:/misc/twitter_brexit/twitter_text_0621_0914_1018pm.txt',
'g:/misc/twitter_brexit/twitter_text_0622_0838_0918am.txt',
'g:/misc/twitter_brexit/twitter_text_0622_1038_1124am.txt',
'g:/misc/twitter_brexit/twitter_text_0622_1201_1243pm.txt',
'g:/misc/twitter_brexit/twitter_text_0622_0425_0546pm.txt',
'g:/misc/twitter_brexit/twitter_text_0622_0731_0821pm.txt',
'g:/misc/twitter_brexit/twitter_text_0622_1003_1044pm.txt',
'g:/misc/twitter_brexit/twitter_text_0623_0837_1027am.txt']

files_out =['g:/misc/twitter_brexit/twitter_text_0621_0126_0153pm.csv',
'g:/misc/twitter_brexit/twitter_text_0621_0306_0420pm.csv',
'g:/misc/twitter_brexit/twitter_text_0621_0516_0547pm.csv',
'g:/misc/twitter_brexit/twitter_text_0621_0914_1018pm.csv',
'g:/misc/twitter_brexit/twitter_text_0622_0838_0918am.csv',
'g:/misc/twitter_brexit/twitter_text_0622_1038_1124am.csv',
'g:/misc/twitter_brexit/twitter_text_0622_1201_1243pm.csv',
'g:/misc/twitter_brexit/twitter_text_0622_0425_0546pm.csv',
'g:/misc/twitter_brexit/twitter_text_0622_0731_0821pm.csv',
'g:/misc/twitter_brexit/twitter_text_0622_1003_1044pm.csv',
'g:/misc/twitter_brexit/twitter_text_0623_0837_1027am.csv']
llist=[]
clist=[]

def get_from_file(filename):
    datadump=[]
    f=open(filename,"r")
    for line in f:
        try:
            x= json.loads(line)
            datadump.append(x)
        except:
            continue
    return datadump

def tweet_overview(filename):
    data = get_from_file(filename)
    contents = []
    t_creat_at = []
    user_id = []
    user_loc = []
    user_hist = []
    hashtags = []
    language = []
    countries = []
    place = []
    for i in range(len(data)):
        tweet = data[i]
        try:
            lan=tweet['lang']
        except KeyError:
            lan=""
        language.append(lan)
        try:
            countries.append(tweet['place']['country'])
        except (TypeError, KeyError):
            countries.append("unknown")
        if lan!='en':
            pass
        else:
            t_creat_at.append(tweet['created_at'])
            contents.append(tweet['text'])
            tags = tweet['entities']['hashtags']
            tt=[x['text'] for x in tags]
            hashtags.append(";".join(tt))
            user_id.append(tweet['user']['id'])
            user_loc.append(tweet['user']['location'])
            user_hist.append(tweet['user']['created_at'])
            try:
                place.append(tweet['place']['country'])
            except (TypeError, KeyError):
                place.append("")
    d = pd.DataFrame()
    d['created_at']= t_creat_at
    d['text']= contents
    d['user_loc'] = user_loc
    d['user_loc2'] = place
    d['user_id'] = user_id
    d['user_hist'] = user_hist
    d['hashtags'] = hashtags
    return (d, language, countries)

def write_csv(data, filename):
    data.to_csv(filename, encoding='utf-8')
    
if __name__ == '__main__':
    for (fin, fout) in zip(files_in,files_out):
        (data, lang, contr)= tweet_overview(fin)
        print fin
        llist = llist+lang
        clist = clist + contr
        write_csv(data, fout)                
    
