--Cleaning data with sql queries


select *
from PortfolioProject.dbo.national_housing
--------------------------------------------------------------------------------------------
--Standardize date format


ALTER TABLE PortfolioProject.dbo.national_housing
ADD SaleDate2 Date;

Update PortfolioProject.dbo.national_housing
SET SaleDate2 = CONVERT(Date, SaleDate)

SELECT SaleDate2
FROM PortfolioProject.dbo.national_housing



----------------------------------------------------------------------------------------------
--Populate property address data

SELECT *
FROM PortfolioProject.dbo.national_housing
--WHERE PropertyAddress is null
ORDER BY ParcelID

SELECT a.ParcelID, a.PropertyAddress, b.ParcelID, b.PropertyAddress, ISNULL(a.PropertyAddress,b.PropertyAddress)
FROM PortfolioProject.dbo.national_housing  a
JOIN PortfolioProject.dbo.national_housing  b
	on a. ParcelID= b.ParcelID 
	and a.[UniqueID] <> b.[UniqueID]
WHERE a.PropertyAddress is null

Update a
SET PropertyAddress= ISNULL(a.PropertyAddress,b.PropertyAddress)
FROM PortfolioProject.dbo.national_housing  a
JOIN PortfolioProject.dbo.national_housing  b
	on a. ParcelID= b.ParcelID 
	and a.[UniqueID] <> b.[UniqueID]
WHERE a.PropertyAddress is null



-----------------------------------------------------------------------------------------------------------
--Spliting the address into individual colomn( Address, city and State)

SELECT PropertyAddress
FROM PortfolioProject.dbo.national_housing
--WHERE PropertyAddress is null
--ORDER BY ParcelID



SELECT 
SUBSTRING (PropertyAddress, 1, CHARINDEX(',',PropertyAddress) -1) as Address,
	SUBSTRING (PropertyAddress, CHARINDEX(',',PropertyAddress) +1, LEN(PropertyAddress)) as Address
FROM PortfolioProject.dbo.national_housing
--WHERE PropertyAddress is null
ORDER BY ParcelID

ALTER TABLE PortfolioProject.dbo.national_housing
Add PropertySpiltAddress Nvarchar (255)

Update PortfolioProject.dbo.national_housing
SET PropertySpiltAddress = SUBSTRING (PropertyAddress, 1, CHARINDEX(',',PropertyAddress) -1)

ALTER TABLE PortfolioProject.dbo.national_housing
Add PropertySpiltCity Nvarchar (255)

Update PortfolioProject.dbo.national_housing
SET PropertySpiltCity = SUBSTRING (PropertyAddress, CHARINDEX(',',PropertyAddress) +1, LEN(PropertyAddress))



-- crosscheck the splited colomns

select *
from PortfolioProject.dbo.national_housing


-- Spliting the owner's address

select OwnerAddress
from PortfolioProject.dbo.national_housing



select 
PARSENAME(REPLACE(OwnerAddress,',','.'),3)
,PARSENAME(REPLACE(OwnerAddress,',','.'),2)
,PARSENAME(REPLACE(OwnerAddress,',','.'),1)
from PortfolioProject.dbo.national_housing



ALTER TABLE PortfolioProject.dbo.national_housing
Add OwnerSpiltAddress Nvarchar (255)

Update PortfolioProject.dbo.national_housing
SET OwnerSpiltAddress = PARSENAME(REPLACE(OwnerAddress,',','.'),3)


ALTER TABLE PortfolioProject.dbo.national_housing
Add OwnerSpiltCity Nvarchar (255)

Update PortfolioProject.dbo.national_housing
SET OwnerSpiltCity = PARSENAME(REPLACE(OwnerAddress,',','.'),2)

ALTER TABLE PortfolioProject.dbo.national_housing
Add OwnerSpiltState Nvarchar (255)

Update PortfolioProject.dbo.national_housing
SET OwnerSpiltState = PARSENAME(REPLACE(OwnerAddress,',','.'),1)

-- crosscheck the splited colomns

select *
from PortfolioProject.dbo.national_housing



----------------------------------------------------------------------------------------------------------
-- let update 'Sold as vacant' colomn: we have 4 distinct values instead of 2


select distinct(SoldAsVacant), COUNT(SoldAsVacant)
from PortfolioProject.dbo.national_housing
group by SoldAsVacant
ORDER BY 2


Select  SoldAsVacant,
	CASE WHEN SoldAsVacant = 'Y' THEN 'Yes'
		 WHEN SoldAsVacant = 'N' THEN 'No'
		 ELSE SoldAsVacant
		 END
from PortfolioProject.dbo.national_housing



UPDATE PortfolioProject.dbo.national_housing
SET SoldAsVacant = CASE WHEN SoldAsVacant = 'Y' THEN 'Yes'
		 WHEN SoldAsVacant = 'N' THEN 'No'
		 ELSE SoldAsVacant
		 END

------------------------------------------------------------------------------------------------

-- Remove duplicates
WITH RowNumCTE as (
SELECT *,
	ROW_NUMBER() OVER(
	PARTITION BY ParcelID,
				 PropertyAddress,
				 SalePrice,
				 SaleDate,
				 LegalReference
				 ORDER BY
					UniqueId
				    ) row_num

from PortfolioProject.dbo.national_housing
--ORDER BY ParcelID
)
DELETE
FROM RowNumCTE
WHERE row_num >1
ORDER BY PropertyAddress



-------------------------------------------------------------------------------------------------------------

---Delete used colomn

SELECT *
FROM PortfolioProject.dbo.national_housing

ALTER TABLE PortfolioProject.dbo.national_housing
DROP COLUMN OwnerAddress, TaxDistrict, PropertyAddress, SaleDate





--------------------------
--The dataset is clean and ready to use

select *
from PortfolioProject.dbo.national_housing