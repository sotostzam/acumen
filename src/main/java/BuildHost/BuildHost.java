package BuildHost;

import com.sun.net.httpserver.Headers;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.Date;


public class BuildHost {
    // define all the variables
    public static int Device_counter = 0;    // the account of device
    // all the sensors data
    public static ArrayList<ArrayList<String[]>> sensors = new ArrayList<ArrayList<String[]>>();
    // define server
    public static HttpServer server;

    public static void start() throws Exception {
        server = HttpServer.create(new InetSocketAddress(8000), 0);
        if (sensors.size() == 0){
            ArrayList<String[]> tempsensor = new ArrayList<String[]>();
            tempsensor.add(0, new String[] {"0","0","0","0","0","0","0"});
            sensors.add(0, tempsensor);
        }
        server.createContext("/test", new MyHandler());
        server.createContext("/get_sensor", new MySensor());
        server.createContext("/index", new MyPage());
        server.createContext("/read_sensor", new ClientData());
        server.setExecutor(null); // creates a default executor
        server.start();
    }

    public static void stop() throws Exception {
        server.stop(0);
        sensors.clear();
    }


    static class MyHandler implements HttpHandler {
        public void handle(HttpExchange t) throws IOException {
            String response = "<!DOCTYPE html><html><head><title></title></head><body><p>TEST!</p></body></html>";
            Headers responseHeaders = t.getResponseHeaders();
            responseHeaders.set("Content-Type", "text/html");
            t.sendResponseHeaders(200, response.length());
            OutputStream os = t.getResponseBody();
            os.write(response.getBytes());
            os.close();
        }
    }

    static class MyPage implements HttpHandler {
        public void handle(HttpExchange t) throws IOException {
            int i;
            StringBuilder data = new StringBuilder();
            String direct = System.getProperty("user.dir");
            String index_page = direct + "/src/main/java/BuildHost/gyroacc_sensor.html";
            InputStream is = new FileInputStream(index_page);
            while((i=is.read())!=-1)
            {
                data.append((char)i);
            }
            Headers responseHeaders = t.getResponseHeaders();
            responseHeaders.set("Content-Type", "text/html");
            t.sendResponseHeaders(200, 0);
            OutputStream os = t.getResponseBody();
            os.write(data.toString().getBytes());
            os.close();
        }
    }

    static class MySensor implements HttpHandler {

        protected Date now;

        public void handle(HttpExchange t) throws IOException {
            now = new Date();

            //dummy data for read_sensor

            //build data with xml format
            String xml_declare  = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";
            String xml_header   = "<data>";

            String xml_device_count   = "<DeviceCount>" + ++Device_counter + "</DeviceCount>";

            String xml_footer   = "</data>";
            String xml_response = xml_declare + xml_header + xml_device_count + xml_footer;

            //set response header
            Headers responseHeaders = t.getResponseHeaders();
            responseHeaders.set("Content-Type", "text/xml");
            t.sendResponseHeaders(200, 0);

            //send response
            OutputStream os = t.getResponseBody();
            os.write(xml_response.getBytes());
            os.close();
        }
    }

    static class ClientData implements HttpHandler {
        public void handle(HttpExchange t) throws IOException {
            int i;
            StringBuilder data = new StringBuilder();
            if(t.getRequestMethod().equalsIgnoreCase("POST")) {
                InputStream is = t.getRequestBody();

                while((i=is.read())!=-1)
                {
                    data.append((char)i);
                }
            }
            String[] sensor_data = data.toString().split(",");
            // get the new data from client
            sensors = insertSensor(sensors, sensor_data);
            /*for (i = 0; i < sensor.size(); i++){
                String[] temp = sensor.get(i);
                System.out.print(i + ":");
                for (j = 0; j< temp.length; j++){
                    System.out.print(temp[j]);
                    if (j != temp.length - 1){
                        System.out.print(",");
                    }
                }
                System.out.println();
            }
            System.out.println();*/

            //set response header
            Headers responseHeaders = t.getResponseHeaders();
            responseHeaders.set("Content-Type", "text/plain");
            t.sendResponseHeaders(200, 0);

            //send response
            String response = "Success";
            OutputStream os = t.getResponseBody();
            os.write(response.getBytes());
            os.close();
        }
        // add the sensor data to sensors Arraylist
        public static ArrayList<ArrayList<String[]>> insertSensor (ArrayList<ArrayList<String[]>> sensors, String data[]){
            int index = Integer.parseInt(data[7]) - 1; // device number
            ArrayList<String[]> tempsensor = new ArrayList<String[]>();
            if (index >= sensors.size()){   // detect a new device
                tempsensor.add(0, new String[]{"0", "0", "0", "0", "0", "0", "0"});
                sensors.add(index, tempsensor);
            }else{
                tempsensor = sensors.get(index);
                tempsensor = getdata(tempsensor, data);
                sensors.set(index, tempsensor);
            }
            return sensors;
        }
        // add the new data to sensor Arraylist
        public static ArrayList<String[]> getdata(ArrayList<String[]> sensor, String data[]){
            int dataAccount = sensor.size();
            if (dataAccount <= 200){
                sensor.add(dataAccount, new String[] {data[0], data[1], data[2], data[3], data[4], data[5], data[6]});
            }else {
                sensor.add(dataAccount, new String[] {data[0], data[1], data[2], data[3], data[4], data[5], data[6]});
                sensor.remove(0);
            }
            return sensor;
        }
    }
}
