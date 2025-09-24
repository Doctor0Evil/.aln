# Example of a simple network monitoring tool in Python
import psutil
import time

def monitor_network():
    while True:
        net_io = psutil.net_io_counters()
        print(f"Bytes Sent: {net_io.bytes_sent}, Bytes Received: {net_io.bytes_recv}")
        time.sleep(1)

if __name__ == "__main__":
    monitor_network()
