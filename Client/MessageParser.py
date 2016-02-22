import json

class MessageParser():
    def __init__(self):

        self.possible_responses = {
            'error': self.parse_error,
            'info': self.parse_info,
	    # More key:values pairs are needed	
        }

    def parse(self, payload):
        payload = json.dumps(payload)# decode the JSON object

        if payload['response'] in self.possible_responses:
            return self.possible_responses[payload['response']](payload)
        else:
            print("response not valid")# temp Response not valid

    def parse_error(self, payload):
        print("parse error") # temp
    
    def parse_info(self, payload):
        print("parese info") # temp
        
    # Include more methods for handling the different responses... 
