const { assertEquals, runScenario } = require('@stacks/clarity-js-sdk');

describe('Decentralized Marketplace Tests', () => {
  const seller = accounts.seller;
  const buyer = accounts.buyer;

  it('should create listing successfully', () => {
    runScenario(([client]) => {
      const result = client.createListing({
        sender: seller,
        title: "Vintage Chair",
        description: "Antique wooden chair",
        price: 1000
      });
      assertEquals(result.success, true);
    });
  });

  it('should allow buying item', () => {
    runScenario(([client]) => {
      const result = client.buyItem({
        sender: buyer,
        listingId: 1
      });
      assertEquals(result.success, true);
    });
  });

  it('should confirm delivery', () => {
    runScenario(([client]) => {
      const result = client.confirmDelivery({
        sender: buyer,
        listingId: 1
      });
      assertEquals(result.success, true);
    });
  });
});
