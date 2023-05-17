
// #pragma GCC optimize ("O0")

#include <iostream>
#include <vector>
#include <memory>
#include <thread>

using std::shared_ptr;
using std::vector;

#define MQTT_STD_VARIANT
#define MQTT_USE_WS

#include <mqtt_client_cpp.hpp>

#include "coordinates.hpp"
#include "algebra.hpp"

// #include <boost/lexical_cast.hpp>

bool mqttIsInitialised = false;
std::shared_ptr<MQTT_NS::callable_overlay<MQTT_NS::async_client<MQTT_NS::ws_endpoint<as::ip::tcp::socket,MQTT_NS::strand>>>> mqttClient_ptr;
boost::asio::io_context	ioc;

void outputMqtt(
	KFState&	kfState)
{
	if (mqttIsInitialised == false)
	{
		return;
	}
	
	for (int dt : {0,1})
	{
		kfState.stateTransition(std::cout, kfState.time + dt * 608400);
		
		for (auto& [kfKey, index] : kfState.kfIndexMap)
		{
			if	( kfKey.type	!= KF::REC_POS
				||kfKey.num		!= 0)
			{
				continue;
			}
			
			Vector3d rRec;
			for (int i = 0; i < 3; i++)
			{
				KFKey posKey = kfKey;
				posKey.num = i;
				kfState.getKFValue(posKey, rRec(i));
			}
			
			double pos[3];
			ecef2pos(rRec, pos);
			
			char data[2048];
			snprintf(data, sizeof(data), "{\"time\": \"%s\", \"coordinates\": [%25.20f, %25.20f], \"rotation\": 0}", kfState.time.to_string().c_str(), pos[1] * R2D, pos[0] * R2D); 
			
			if (pos[0] * R2D < -10)
			if (pos[1] * R2D > 112)
			{
				mqttClient_ptr->async_publish("site/" + kfKey.str,	data,		MQTT_NS::qos::exactly_once | MQTT_NS::retain::yes);
			}
		}
		
	}
}

void mqttoooo() 
{
	MQTT_NS::setup_log();

	uint16_t port = 9001;

// 	auto c = MQTT_NS::make_sync_client(ioc, "localhost", port);
	mqttClient_ptr = MQTT_NS::make_async_client_ws(ioc, "localhost", port);

	auto& c = mqttClient_ptr;
	
	using packet_id_t = typename std::remove_reference_t<decltype(*c)>::packet_id_t;
	
	// Setup client
	c->set_client_id("ginan");
	c->set_clean_session(true);

	uint16_t pid_sub3 = c->acquire_unique_packet_id();
	
	// Setup handlers
	c->set_connack_handler(		[&c, &pid_sub3](bool sp, MQTT_NS::connect_return_code connack_return_code)
	{
		std::cout << "[client] Connack handler called"	<< std::endl;
		std::cout << "[client] Session Present: "		<< std::boolalpha << sp << std::endl;
		std::cout << "[client] Connack Return Code: "	<< MQTT_NS::connect_return_code_to_str(connack_return_code) << std::endl;
					  
		if (connack_return_code == MQTT_NS::connect_return_code::accepted)
		{
			mqttIsInitialised = true;
// 			pid_sub1 = c->subscribe("mqtt_client_cpp/topic1",	MQTT_NS::qos::at_most_once);
// 			pid_sub3 = c->subscribe("iss",						MQTT_NS::qos::at_least_once);
// 		c->publish("mqtt_client_cpp/topic2_1", "test2_1", MQTT_NS::qos::at_least_once);
		c->async_publish(
                    "mqtt_client_cpp/topic1",
                    "test1",
                    MQTT_NS::qos::exactly_once,
                    // [optional] checking async_publish completion code
                    []
                    (MQTT_NS::error_code ec){
                        std::cout << "async_publish callback: " << ec.message() << std::endl;
                    }
                );
		}
//                 c->async_subscribe(
//                     pid_sub3,
//                     "iss",
//                     MQTT_NS::qos::at_most_once/*,
//                     // [optional] checking async_subscribe completion code
//                     []
//                     (MQTT_NS::error_code ec)
// 					{
//                         std::cout << "async_subscribe callback: " << ec.message() << std::endl;
//                     }*/
//                 );
		
		return true;
	});
	
	c->set_close_handler(	[]		()								{	std::cout << "[client] closed."					<< std::endl;	});
	c->set_error_handler(	[]		(MQTT_NS::error_code	ec)		{	std::cout << "[client] error: " << ec.message()	<< std::endl;	});
	c->set_puback_handler(	[&]		(packet_id_t packet_id)	{	std::cout << "[client] puback received. packet_id: "	<< packet_id << std::endl;		return true;	});
	c->set_pubrec_handler(	[&]		(packet_id_t packet_id)	{	std::cout << "[client] pubrec received. packet_id: "	<< packet_id << std::endl;		return true;	});
	c->set_pubcomp_handler(	[&]		(packet_id_t packet_id)	{	std::cout << "[client] pubcomp received. packet_id: "	<< packet_id << std::endl;		return true;	});

	c->set_suback_handler(
		[&]
		(packet_id_t packet_id, vector<MQTT_NS::suback_return_code> results)
	{
// 		std::cout << "[client] suback received. packet_id: " << packet_id << std::endl;
// 		for (auto const& e : results)
// 		{
// 			std::cout << "[client] subscribe result: " << e << std::endl;
// 		}
// 		
// 		if (packet_id == pid_sub1)
// 		{
// 			c->publish("mqtt_client_cpp/topic1", "test1", MQTT_NS::qos::at_most_once);
// 		}
// 		else if (packet_id == pid_sub2)
// 		{
// 			c->publish("mqtt_client_cpp/topic2_1", "test2_1", MQTT_NS::qos::at_least_once);
// 			c->publish("mqtt_client_cpp/topic2_2", "test2_2", MQTT_NS::qos::exactly_once);
// 		}
// 		
// 		std::cout << std::endl;
		return true;
	});
	
	c->set_publish_handler(
		[&]
		(MQTT_NS::optional<packet_id_t>	packet_id,
		 MQTT_NS::publish_options		pubopts,
		 MQTT_NS::buffer				topic_name,
		 MQTT_NS::buffer				contents)
	{
		std::cout
		<< "[client] publish received. "
		<< " dup: "    << pubopts.get_dup()
		<< " qos: "    << pubopts.get_qos()
		<< " retain: " << pubopts.get_retain() << std::endl;
		
		if (packet_id)
			std::cout << "[client] packet_id: "		<< *packet_id << std::endl;
		std::cout << "[client] topic_name: "		<< topic_name << std::endl;
		std::cout << "[client] contents: "			<< contents << std::endl;
		std::cout << std::endl;
		
		return true;
	});

	// Connect
// 	c->connect();
	mqttClient_ptr->async_connect(
		// [optional] checking underlying layer completion code
		[]
		(MQTT_NS::error_code ec)
		{
			std::cout << "async_connect callback: " << ec.message() << std::endl;
		}
	);
// 	c->disconnect();

	
		std::thread([](){ioc.run();}).detach();
// 	ioc.run();
}
