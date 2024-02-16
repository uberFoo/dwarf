import aiohttp
import asyncio

async def fetch_url(session, url):
    async with session.get(url) as response:
        return await response.text()

async def main():
    n = 100
    url = 'http://www.rust-lang.org'
    urls = [url] * n

    async with aiohttp.ClientSession() as session:
        tasks = [fetch_url(session, url) for url in urls]
        responses = await asyncio.gather(*tasks)
        for url, response in zip(urls, responses):
            # print(f'Response from {url}: {response[:100]}...')  # Print first 100 characters of each response
            print(response)

# Run the main function
asyncio.run(main())