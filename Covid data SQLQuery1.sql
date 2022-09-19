SELECT *
FROM PortfolioProject..CovidDeaths
where continent is not null
order by 3,4

--SELECT *
--FROM PortfolioProject..CovidVaccinations
--order by 3,4

--selecting the data

select location, date, total_cases, new_cases, total_deaths, population
FROM PortfolioProject..CovidDeaths
order by 1,2



---Total cases vs TOtal deaths
---shows the likelihood of dieing if you contract COvid in Nigeria
select location, date, total_cases, total_deaths, (total_deaths/total_cases)*100 as death_percentage
FROM PortfolioProject..CovidDeaths
where location like 'Nigeria'
order by 1,2


---Total cases vs popuplation
--shows the percentage of population that got covid

select location, date, population, total_cases, (total_cases/population)*100 as population_infected_percentage
FROM PortfolioProject..CovidDeaths
where location like 'Nigeria'
order by 1,2


--Countries with the highest infection rate compared to population
select location, population, max(total_cases) as highest_infection_count, max((total_cases/population))*100 as population_infected_percentage
FROM PortfolioProject..CovidDeaths
--where location like 'Nigeria'
group by location, population
order by population_infected_percentage desc


---number of death By continent per polulation
select continent, max(cast(total_deaths as int)) as total_death_count
FROM PortfolioProject..CovidDeaths
where continent is not  null
group by continent
order by total_death_count desc



--countries with the highest death count per population
select location, max(cast(total_deaths as int)) as total_death_count
FROM PortfolioProject..CovidDeaths
where continent is  null
group by location
order by total_death_count desc



--by adding continent to group by  and select clause

--Global numbers
--total no. of cases, total deaths and death percentage

select sum(new_cases) total_cases, sum(cast(new_deaths as int)) as total_deaths, 
sum(cast(new_deaths as int))/sum(new_cases)*100 as death_percentage
FROM PortfolioProject..CovidDeaths
--where location like 'Nigeria'
where continent is not null
order by 1,2



--merging the both dataset and finding the total population vs vaccination
select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations,
SUM(CONVERT(int,vac.new_vaccinations)) OVER (Partition by dea.location ORDER BY dea.location,
dea.date) as rolling_people_vaccinated
from PortfolioProject..CovidDeaths dea
join PortfolioProject..CovidVaccinations vac
	on dea.location= vac.location
	and dea.date = vac.date
where dea.continent is not null
order by 2,3

--CTE 

WITH PopvsVac ( continent, location, data, population, new_vaccinations,rolling_people_vaccinated)
as
(
select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations,
SUM(CONVERT(int,vac.new_vaccinations)) OVER (Partition by dea.location ORDER BY dea.location,
dea.date) as rolling_people_vaccinated
from PortfolioProject..CovidDeaths dea
join PortfolioProject..CovidVaccinations vac
	on dea.location= vac.location
	and dea.date = vac.date
where dea.continent is not null
--order by 2,3
)


-- temp table




DROP Table if exists #percent_population_vaccinated
Create Table #percent_population_vaccinated
(
continent nvarchar(255),
loaction nvarchar(255),
Date datetime,
population numeric,
new_vaccinations numeric,
rolling_people_vaccinated numeric
)
Insert into #percent_population_vaccinated
select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations,
SUM(CONVERT(int,vac.new_vaccinations)) OVER (Partition by dea.location ORDER BY dea.location,
dea.date) as rolling_people_vaccinated
from PortfolioProject..CovidDeaths dea
join PortfolioProject..CovidVaccinations vac
	on dea.location= vac.location
	and dea.date = vac.date
where dea.continent is not null
--order by 2,3

SELECT *, (rolling_people_vaccinated/population)*100
FROM #percent_population_vaccinated

-- creating view to store later for visualizations

Create View percent_population_vaccinated as
select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations,
SUM(CONVERT(int,vac.new_vaccinations)) OVER (Partition by dea.location ORDER BY dea.location,
dea.date) as rolling_people_vaccinated
from PortfolioProject..CovidDeaths dea
join PortfolioProject..CovidVaccinations vac
	on dea.location= vac.location
	and dea.date = vac.date
where dea.continent is not null
--order by 2,3


Select *
from percent_population_vaccinated