ALTER TABLE blocked_ips ADD UNIQUE unique_ip_range(ip_start, ip_end);
