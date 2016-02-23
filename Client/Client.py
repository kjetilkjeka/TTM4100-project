# -*- coding: utf-8 -*-
import json
import socket
from MessageReceiver import MessageReceiver
from MessageParser import MessageParser

class Client:
    """
    This is the chat client class
    """

    def __init__(self, host, server_port):
        """
        This method is run when creating a new Client object
        """
        # initialization
        self.server_port = server_port
        self.host = host
        
        # Set up the socket connection to the server
        self.connection = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        # Initiate the connection to the server
        self.connection.connect((self.host, self.server_port))

        # Start the message receiver
        self.message_receiver = MessageReceiver(self, self.connection)
        
        # TODO: Finish init process with necessary code
        self.run()

    def run(self):

        while True:
            command_text = raw_input('Enter command: ')
            command_list = command_text.split(" ")
            request = command_list[0]

            if(len(command_list) >= 2):
                content = command_list[1]
            else:
                content = "None"
            
            self.send_message(request, content)
            
    def disconnect(self):
        # TODO: Handle disconnection
        pass

    def receive_message(self, message):
        # TODO: Handle incoming message
        # This is realy a formating function
        print(message)

    def print_user_list(self, user_list):
        # called by message_receiver
        pass

    def print_help(self, helptext):
        # Print a helptext received from server
        pass

    def send_message(self, request, content):
        payload = json.dumps({'request':request, 'content':content})
        self.connection.send(payload)
        pass
        
    # More methods may be needed!


if __name__ == '__main__':
    """
    This is the main method and is executed when you type "python Client.py"
    in your terminal.

    No alterations are necessary
    """
    client = Client('localhost', 9998)
