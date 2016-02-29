import json

class MessageParser():
    def __init__(self):

        self.possible_responses = {
            'error': self.parse_error,
            'info': self.parse_info,
            'message': self.parse_message,
            'history': self.parse_history,
	    # More key:values pairs are needed	
        }

    def parse(self, payload):
        payload = json.loads(payload)# decode the JSON object

        if payload['response'] in self.possible_responses:
            return self.possible_responses[payload['response']](payload)
        else:
            print("response not valid")# temp Response not valid

    def parse_message(self, payload):
        return payload['content']

    def parse_history(self, payload):
        message_list = payload['content']
        message_string = ''
        for message in message_list:
            message_string = message_string + '\n' +  message['content']
        return message_string
    
    def parse_error(self, payload):
        print("parse error") # temp
    
    def parse_info(self, payload):
        print("parese info") # temp
        
    # Include more methods for handling the different responses... 
