# Affinity API Client 

A scala client for interacting with the [Affinity](https://www.affinity.co) API. 

## Usage

```scala

val client = AffinityClient(YOUR_API_KEY)

client.getLists()                  # => Future[Seq[List]]
client.searchPeople(Some("Bob"))   # => Future[PersonSearchResponse]
client.getOrg(12345)               # => Future[Org]

``` 

See `AffinityClient.scala` for more details. 



