
# Download TUB GTFS data
# tub_url = "https://tub.pt/developer/gtfs/feed/tub.zip" # 1.7MB
# download.file(tub_url, destfile = "original/tub_gtfs.zip")

# Read the GTFS data
tub_path = "original/tub_gtfs.zip"
TUB_original = gtfsrouter::extract_gtfs(tub_path)

# Create the missing transfers.txt file
tub_transfers = gtfsrouter::gtfs_transfer_table(TUB_original)
write.csv(tub_transfers[["transfers"]], "original/transfers.txt", row.names = F, quote = F)
# include this file again in the original zip