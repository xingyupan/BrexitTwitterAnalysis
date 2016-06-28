from tweepy.streaming import StreamListener
from tweepy import OAuthHandler
from tweepy import Stream

#Variables that contains the user credentials to access Twitter API 
access_token = "35183885-MjolEbYE3GeGg4MX3c0hoRr6qtVrz6W6C4nITAgyj"
access_token_secret = "hhV1IcNUqXZ9Ll8QuKbDlztgcqxodkaItSMSBxho4GwL7"
consumer_key = "nduGFFaH20iTnRhgTvw4D9QVF"
consumer_secret = "Ne8WlXy2Dv2mtveERmAYL2oVij4VBPWptxng6UvAORoCNo45CR"


#This is a basic listener that just prints received tweets to stdout.
class StdOutListener(StreamListener):

    def on_data(self, data):
        print data
        return True

    def on_error(self, status):
        print status


if __name__ == '__main__':

    #This handles Twitter authetification and the connection to Twitter Streaming API
    l = StdOutListener()
    auth = OAuthHandler(consumer_key, consumer_secret)
    auth.set_access_token(access_token, access_token_secret)
    stream = Stream(auth, l)

    #This line filter Twitter Streams to capture data by the keywords: 'python', 'javascript', 'ruby'
    stream.filter(track=['EURef', 'brexit', 'voteleave','votestay'])
