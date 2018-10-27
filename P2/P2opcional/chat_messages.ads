package Chat_Messages is
	type Message_Type is (Init, Writer, Server, Logout, Collection_Request,
									Collection_Data, Ban, Shutdown);
end Chat_Messages;
